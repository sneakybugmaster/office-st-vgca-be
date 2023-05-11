package com.vz.backend.business.service;

import com.vz.backend.business.domain.Attachment;
import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.dto.hstl.ecm.FolderAttachmentDto;
import com.vz.backend.business.repository.IAttachmentRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AttachmentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.core.service.FilesStorageService;
import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class AttachmentService extends BaseService<Attachment> {
    @Autowired
    FilesStorageService storageService;
    @Autowired
    DocumentService docService;
    @Autowired
    EncryptionService encryptService;
    @Autowired
    PdfService pdfService;
    @Autowired
    private IAttachmentRepository attachmentRepository;

    @Override
    public IRepository<Attachment> getRepository() {
        return attachmentRepository;
    }

    public List<Attachment> addListAttachment(MultipartFile[] files, Long objId) {
        if (ArrayUtils.isEmpty(files) || files == null || files.length == 0) {
            return Collections.emptyList();
        }
        List<Attachment> aList = new ArrayList<>();
        Documents document = null;

        Optional<Documents> documentOptional = docService.findById(objId);
        if (documentOptional.isPresent()) {
            document = documentOptional.get();
        }

        for (int i = 0; i < files.length; i++) {
            Attachment a = new Attachment();
            MultipartFile f = files[i];
            if (i == 0) {
                try {
                    if (document != null && document.getAutoCreateSeal()) {
                        f = pdfService.addWatermarkToPDF(f, "ĐẾN", document.getNumberArrivalStr(), document.getDateIssued());
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                    throw new RestExceptionHandler(e.getMessage());
                }
            }
            a.setName(storageService.save(f));
            a.setType(f.getContentType());
            a.setSize(f.getSize());
            a.setDocumentId(objId);
            aList.add(a);
        }
        attachmentRepository.saveAll(aList);
        return aList;
    }

    public Attachment addAttachment(MultipartFile file, Long objId, Long clientId, Long userId) {
        Attachment a = new Attachment();
        a.setName(storageService.save(file));
        a.setType(file.getContentType());
        a.setSize(file.getSize());
        a.setDocumentId(objId);
        a.setClientId(clientId);
        a.setCreateBy(userId);
        return attachmentRepository.save(a);
    }

    public Attachment update(Attachment a, File f) {
        a.setName(f.getName());
        a.setType(FilenameUtils.getExtension(f.getName()));
        a.setSize(f.length());
        return attachmentRepository.save(a);
    }

    public Attachment save(Long docId, com.vz.backend.business.dto.outsideconnect.Attachment f) {
        Attachment a = new Attachment();
        a.setName(f.getName());
        a.setType(FilenameUtils.getExtension(f.getName()));
        a.setSize(f.getSize());
        a.setDocumentId(docId);
        return attachmentRepository.save(a);
    }

    public Attachment save(Long docId, File f) {
        Attachment a = new Attachment();
        a.setName(f.getName());
        a.setType(FilenameUtils.getExtension(f.getName()));
        a.setSize(f.length());
        a.setDocumentId(docId);
        a.setAtmType(AttachmentTypeEnum.RESOLVED_FILE);
        return attachmentRepository.save(a);
    }

    public Attachment getByDocIdAndType(Long docId) {
        return attachmentRepository.findFirstByClientIdAndDocumentIdAndActiveTrue(BussinessCommon.getClientId(), docId);
    }

    public List<Attachment> addListAttachment(List<Attachment> listAtt, List<Documents> objId) {
        List<Attachment> aList = new ArrayList<>();
        if (objId == null || objId.isEmpty()) {
            return aList;
        }
        for (Documents doc : objId) {
            for (Attachment att : listAtt) {
                if (AttachmentTypeEnum.RESOLVED_FILE.equals(att.getAtmType())) {
                    continue;
                }
                String originalFileName = FilesStorageService.origin(att.getName());
                String newFileName = FilesStorageService.parse(originalFileName);
                storageService.copyFile(att.getName(), newFileName);
                Attachment a = new Attachment();
                a.setName(newFileName);
                a.setType(att.getType());
                a.setSize(att.getSize());
                a.setDocumentId(doc.getId());
                aList.add(a);
            }
        }
        return attachmentRepository.saveAll(aList);
    }

    @Transactional
    public void deleteAllByDocId(Long documentId) {
        try {
            attachmentRepository.deleteByDocId(documentId);
        } catch (Exception e) {
            e.printStackTrace();
        }

    }

    public Attachment updateFile(Long id, MultipartFile file) {
        Optional<Attachment> docOptional = this.findById(id);
        if (!docOptional.isPresent()) {
            throw new RestExceptionHandler("Can't find attachment with id: " + id);
        }
        Attachment documentOutAttachment = docOptional.get();
        if (!BussinessCommon.getUser().getId().equals(documentOutAttachment.getCreateBy())) {
            throw new RestExceptionHandler("You are not the owner of this file");
        }
        String oldName = documentOutAttachment.getName();
        String newName = storageService.replace(file, oldName);
        documentOutAttachment.setName(newName);
        return attachmentRepository.save(documentOutAttachment);
    }

    @Override
    public void deleteById(Long id) {
        Optional<Attachment> optional = this.findById(id);
        if (!optional.isPresent()) {
            throw new RestExceptionHandler("Can't find attachment with id: " + id);
        }

        Attachment a = optional.get();
        if (!BussinessCommon.getUser().getId().equals(a.getCreateBy())) {
            throw new RestExceptionHandler("You are not the owner of this file");
        }

        String oldName = a.getName();
        if (Boolean.TRUE.equals(a.getEncrypt())) {
            encryptService.delEncrypt(oldName);
        } else {
            storageService.deleteFile(oldName);
        }
        attachmentRepository.delete(a);
    }

    public Attachment validDownloadFile(String name) {
        Attachment a = validFileByName(name);
//		if (!docService.checkPermission(a.getDocumentId())) {
//			throw new RestExceptionHandler(Message.ATTACHMENT_FILE_NOT_PERMIT);
//		}
        return a;
    }

    public Attachment validFileByName(String name) {
        Attachment files = findFileByName(name);
        if (files == null) {
            throw new RestExceptionHandler(Message.ATTACHMENT_FILE_NOT_FOUND);
        }
        return files;
    }

    public Attachment findFileByName(String name) {
        List<Attachment> lstAttachmentList = attachmentRepository.findByListNameAndClientIdAndActive(name, BussinessCommon.getClientId(), true);
        return lstAttachmentList != null ? lstAttachmentList.get(0) : null;
    }

    public Attachment add(MultipartFile file) {
        Attachment a = new Attachment(file);
        a.setName(storageService.save(file));
        a = attachmentRepository.save(a);
        return a;
    }

    public Attachment update(Long attId, Long objId, MultipartFile file) {
        Attachment a = valid(attId, Message.NOT_FOUND_FILES);
        a.setName(storageService.replace(file, a.getName()));
        a.setDocumentId(objId);
        return attachmentRepository.save(a);
    }

    public void cloneFile(DocumentOut docOut, Long docId) {
        List<DocumentOutAttachment> attachments = docOut.getAttachments().stream()
                .filter(i -> AttachmentTypeEnum.DRAFT.equals(i.getAttachmentType())).collect(Collectors.toList());
        String nName;
        for (DocumentOutAttachment i : attachments) {
            nName = FilesStorageService.parse(i.getDisplayName());
            storageService.copyFile(i.getName(), nName);
            Attachment attachment = new Attachment(nName, i.getType(), i.getSize(), docId);
            attachment.setEncrypt(i.getEncrypt());
            attachment.setName(i.getName());
            attachmentRepository.save(attachment);
        }
    }

    public Attachment add(MultipartFile file, Long objId, String name) {
        docService.valid(objId, Message.NOT_FOUND_DOC);
        return attachmentRepository.save(new Attachment(file, objId, name));
    }

    public List<Attachment> findByObjId(Long objId) {
        return attachmentRepository.findByDocumentIdAndClientIdAndActiveTrue(objId, BussinessCommon.getClientId());
    }

    public List<FolderAttachmentDto> getByFolderIds(List<Long> folderIds) {
        return attachmentRepository.getByFolderIds(folderIds, BussinessCommon.getClientId());
    }
}
