package com.vz.backend.business.thread;

import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.dto.CustomMultipartFile;
import com.vz.backend.business.dto.DocOutSyncDto;
import com.vz.backend.business.dto.DocOutValueDateSyncDto;
import com.vz.backend.business.dto.DocOutValueSyncDto;
import com.vz.backend.business.exception.NumberOrSignExistsException;
import com.vz.backend.business.repository.IClericalOrgRepository;
import com.vz.backend.business.repository.IDocumentRepository;
import com.vz.backend.business.repository.IDocumentUserRepository;
import com.vz.backend.business.repository.IObjectReadRepository;
import com.vz.backend.business.service.AttachmentService;
import com.vz.backend.business.service.CommonService;
import com.vz.backend.business.service.DocumentService;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.repository.ICategoryRepository;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.multipart.MultipartFile;

import javax.persistence.EntityNotFoundException;
import java.io.IOException;
import java.nio.file.*;
import java.util.*;

@Slf4j
@Component
@Transactional
public class SyncFileVBDen implements Runnable {
    @Value("${configs.dir-sync-vb}")
    private String syncDir;

    @Value("${configs.main-office-org-id}")
    private Long mainOfficeOrgId;

    @Value("${configs.client-id}")
    private Long clientId;

    @Value("${configs.default-sender-id}")
    private Long defaultSenderId;

    @Value("${configs.default-doc-type-id}")
    private Long defaultDocTypeId;


    @Value("${ocr.domain}")
    private String domain;

    @Autowired
    private FilesStorageService filesStorageService;

    @Autowired
    private ICategoryRepository categoryRepository;
    @Autowired
    private IDocumentRepository documentRepository;
    @Autowired
    private DocumentService documentService;

    @Autowired
    private IClericalOrgRepository clericalOrgRepository;
    @Autowired
    private IDocumentUserRepository documentUserRepository;
    @Autowired
    private IObjectReadRepository objectReadRepository;
    @Autowired
    private AttachmentService attachmentService;

    @Autowired
    private CommonService commonService;

    private static Date parseDate(DocOutValueDateSyncDto dateDto) {
        Date date;
        try {
            date = DateTimeUtils.createDate(
                    Integer.parseInt(dateDto.getNam()),
                    Integer.parseInt(dateDto.getThang()),
                    Integer.parseInt(dateDto.getNgay())
            );
        } catch (NumberFormatException e) {
            date = DateTimeUtils.getCurrentTime();
        }
        return date;
    }

    @Override
    public void run() {
        try {
            if (!StringUtils.isNullOrEmpty(syncDir)) {
                listFilesUsingDirectoryStream(syncDir);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public HashMap<String, Path> listFilesUsingDirectoryStream(String dir) throws IOException {
        List<User> clericalListByOrg = clericalOrgRepository.getClericalByOrgIdAndClientId(mainOfficeOrgId, clientId);
        User clerical;
        if (clericalListByOrg.size() > 0) {
            clerical = clericalListByOrg.get(0);
        } else {
            throw new RuntimeException(String.format("Clerical cannot be found for orgId %s", mainOfficeOrgId));
        }

        HashMap<String, Path> hashMap = new HashMap<>();
        try (DirectoryStream<Path> stream = Files.newDirectoryStream(Paths.get(dir))) {
            for (Path path : stream) {
                if (!Files.isDirectory(path)) {
                    /*File name được định nghĩa là số đến của văn bản*/
                    String fileName = path.getFileName().toString();

                    if (!filesStorageService.getFile(fileName).isFile()) {
                        /*Thực hiện tạo vb theo file đang sync*/

                        if (fileName.isEmpty() || !fileName.toLowerCase().endsWith(".pdf")) {
                            moveFileCreateDirectory(dir, path, fileName, "failed");
                            continue;
                        }

                        Resource resource = new UrlResource(path.toUri());
                        // Call OCR lấy nội dung văn bản
                        LinkedMultiValueMap<String, Object> parts = new LinkedMultiValueMap<>();
                        parts.add("file", resource);

                        HttpHeaders httpHeaders = new HttpHeaders();
                        httpHeaders.setContentType(MediaType.MULTIPART_FORM_DATA);

                        HttpEntity<LinkedMultiValueMap<String, Object>> requestEntity = new HttpEntity<>(parts, httpHeaders);

                        RestTemplate restTemplate = new RestTemplate();
                        DocOutSyncDto object = restTemplate.postForObject(domain, requestEntity, DocOutSyncDto.class);
                        // End Call OCR lấy nội dung văn bản
                        // add thông tin vào document thành văn bản đến tiếp nhận

                        Category placeSend = null;
                        DocOutValueSyncDto value = object.getValue();
                        log.info("Document OCR value {}", value);
                        String issuedOrgName = value.getCo_quan_ban_hanh();

                        boolean hasIssuedOrgName = org.springframework.util.StringUtils.hasText(issuedOrgName);
                        if (!hasIssuedOrgName) {
                            log.info("OCR failed to read issued Organization name for file {} ", path);
                            moveFileCreateDirectory(dir, path, fileName, "failed");
                            continue;
                        }

                        DocOutValueDateSyncDto issuedDateDto = value.getNgay_ban_hanh();
                        DocOutValueDateSyncDto receivedDateDto = value.getNgay_den();

                        String summary;
                        if (value.getTrich_yeu() != null) {
                            summary = value.getTrich_yeu();
                        } else {
                            summary = "Trích yếu mặc định";
                        }

                        Category documentType = null;
                        if (value.getTen_van_ban() != null) {
                            documentType = categoryRepository.findByNameAndCodeAndClientIdCaseInsensitive(value.getTen_van_ban(), "LVB", clientId);
                        }

                        // Try parse String to Date. If an exception throws, set Date to current Date.
                        Date issuedDate = parseDate(issuedDateDto);
                        Date arrivalDate = parseDate(receivedDateDto);

                        Documents document = new Documents();
                        document.setIsImported(true);
                        if (placeSend != null && placeSend.getId() != null) {
                            document.setPlaceSendId(placeSend.getId());
                        }
                        document.setDateArrival(issuedDate);
                        document.setDateIssued(arrivalDate);
                        document.setReceivedDate(arrivalDate);
                        document.setNumberOrSign(value.getSo_ky_hieu());
                        document.setDocTypeId(documentType != null ? documentType.getId() : defaultDocTypeId);
                        document.setPreview(summary);
                        document.setCreateBy(clerical.getId());
                        document.setClientId(clientId);

                        log.info("document number or sign {}", document.getNumberOrSign());

                        document = documentService.createDocument(false, document, clerical, clientId, mainOfficeOrgId, false, false, false);
                        document.setStatus(DocumentStatusEnum.WAIT_RECEIVE);

                        documentRepository.save(document);

                        MultipartFile multipartFile = new CustomMultipartFile(resource, fileName, fileName, "application/pdf");

                        commonService.createDocumentUserAndObjectRead(clerical, document, clientId);

                        attachmentService.addAttachment(multipartFile, document.getId(), clientId, clerical.getId());

                        moveFileCreateDirectory(dir, path, fileName, "imported");
                        //Files.delete(path);
                        log.info("file {}, successfully imported", fileName);

                    }
                }
            }
        }
        return hashMap;
    }

    private static void moveFileCreateDirectory(String targetPart, Path source, String targetFileName, String... targetPathMore) throws IOException {
        Path targetFolder = Paths.get(targetPart, targetPathMore);
        if (!Files.exists(targetFolder)) {
            Files.createDirectory(targetFolder);
        }

        Files.move(source, Paths.get(String.valueOf(targetFolder), targetFileName), StandardCopyOption.REPLACE_EXISTING);
    }


}
