package com.vz.backend.business.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.vz.backend.business.config.DocumentCommentTypeEnum;
import com.vz.backend.business.domain.*;
import com.vz.backend.business.domain.BpmnModel2.TYPE_DOCUMENT;
import com.vz.backend.business.dto.*;
import com.vz.backend.business.dto.document.ButtonDto;
import com.vz.backend.business.dto.document.ResolvedDocumentDto;
import com.vz.backend.business.dto.document.ResolvedUserDto;
import com.vz.backend.business.repository.IAttachmentRepository;
import com.vz.backend.business.repository.IClericalOrgRepository;
import com.vz.backend.business.repository.IDocumentRepository;
import com.vz.backend.business.repository.INodeRepository2;
import com.vz.backend.business.service.hstl.HsFolderService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.*;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.CategoryInitDto;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.repository.IUserRepository;
import com.vz.backend.core.service.*;
import com.vz.backend.core.util.StreamUtils;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.util.ResourceUtils;
import org.springframework.web.multipart.MultipartFile;
import org.wickedsource.docxstamper.DocxStamper;
import org.wickedsource.docxstamper.DocxStamperConfiguration;

import java.io.*;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.*;
import java.util.stream.Collectors;

import static com.vz.backend.core.config.Constant.INCOMING_STRING_LITERAL;

@Service
@Slf4j
@RequiredArgsConstructor
public class DocumentService extends BaseService<Documents> {

    @Autowired
    IDocumentRepository docRepository;
    @Autowired
    CategoryService categoryService;
    @Autowired
    DocumentCommentService cmtService;
    @Autowired
    AttachmentCommentService attachCmtService;
    @Autowired
    UserService userService;
    @Autowired
    RoleService rService;
    @Autowired
    DocumentBookService dbService;
    @Autowired
    NotificationService notiService;
    @Autowired
    DocumentInProcessService processService;
    @Autowired
    DocumentInTrackingService trackingServices;
    @Autowired
    DelegateService delegateService;
    @Autowired
    OrganizationService orgService;
    @Autowired
    INodeRepository2 nodeRepo;
    @Autowired
    DocumentUserService docUserService;
    @Autowired
    AttachmentService attService;
    @Autowired
    ObjectReadService objReadService;
    @Autowired
    DocumentOutTrackingService docOutTrackingService;
    @Autowired
    OrganizationService organizationService;
    @Autowired
    INodeRepository2 nodeRepository2;
    @Autowired
    IAttachmentRepository attachmentRepository;
    @Autowired
    private PdfService pdfService;
    @Value("${configs.doc-in.internal-remove: false}")
    private boolean docInternalRemove;
    @Value("${configs.doc-in.return-previous-node: false}")
    private boolean returnPreviousNode;
    @Value("${configs.doc-in.set-deadline: false}")
    private boolean setDeadline;
    @Value("${configs.doc-in.review-required: false}")
    private boolean reviewRequired;
    @Value("${configs.doc-book.org_config: false}")
    private boolean orgConfig;
    @Value("${configs.clerical-org: false}")
    private boolean clericalOrg;
    @Value("${configs.doc-in.search-expired-by-process-deadline: false}")
    private boolean searchByProcessDeadline;
    @Value("${configs.common.encrypt-files: false}")
    private boolean encryptFiles;
    @Value("${configs.doc-in.auto-add-lead-transfer: false}")
    private boolean autoAddLeadTransfer;
    @Value("${configs.doc-in.main-require: false}")
    private boolean mainRequire;
    @Value("${configs.genarate-phieu-trinh: false}")
    private boolean isPhieuTrinh;
    @Autowired
    private IUserRepository userRepository;
    @Autowired
    private RoleService roleService;
    @Autowired
    private DocumentOutService docOutService;
    @Autowired
    private TaskService taskService;
    @Autowired
    private DocumentInManipulationService manipulationService;
    @Autowired
    private HsFolderService hsFolderService;
    @Autowired
    private ClericalOrgService clericalOrgService;
    @Autowired
    private Calendar2Service calendarService;
    @Autowired
    private AuthorityUserService authorityService;
    @Autowired
    private FilesStorageService fileService;
    @Autowired
    private ReportFieldService rpFieldService;
    @Autowired
    private BpmnService2 bpmnService;
    @Autowired
    private IClericalOrgRepository clericalOrgRepo;

    @Autowired
    private NotificationService noticeService;

    private Path rootPath = Paths.get("uploads");

    @Autowired
    private FTPService ftpService;

    @Value("${ftpConfigs.active:false}")
    private boolean ftp;
    @Autowired
    private BpmnService2 bpmnService2;
    @Autowired
    private StraceSystemService straceService;
    @Autowired
    private DocumentReceiveService documentReceiveService;

    private final ObjectReadService objectReadService;

    private Set<Long> all = new HashSet<>();

    private final Map<Long, Category> tmpCategoryMap = new HashMap<>();

    private Category getTmpCategoryById(Long id) {
        if (!tmpCategoryMap.containsKey(id)) {
            Optional<Category> categoryOptional = categoryService.findById(id);
            if (categoryOptional.isPresent()) {
                tmpCategoryMap.put(id, categoryOptional.get());
            } else {
                tmpCategoryMap.put(id, null);
            }
        }
        return tmpCategoryMap.get(id);
    }


    @SuppressWarnings("unchecked")
    public static <T> List<T> getObjStatusByType(Integer typeHandle, Integer statusHandle, boolean vanthu, boolean returnPreviousNode, String rsType) {
        List<DocumentStatusEnum> docStatus = null;
        List<DocumentInHandleStatusEnum> handleStatusList = null;
        List<HandleTypeEnum> handleType = null;
        if (typeHandle.intValue() == 11) { // processing
            docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
            handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DANG_XU_LY", "XIN_DANH_GIA", "DG_CHAP_NHAN", "DG_TU_CHOI", "DA_XU_LY_ADD_USER", "THU_HOI_HOAN_THANH");
            if (vanthu) {
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DANG_XU_LY", "DA_XU_LY", "DA_XU_LY_ADD_USER", "THU_HOI_HOAN_THANH");
            }
        } else if (typeHandle.intValue() == 12) { // done
            handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI", "DA_XU_LY_SWITCH");
            docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC, DocumentStatusEnum.DONE);
            if (vanthu) {
                docStatus = Arrays.asList(DocumentStatusEnum.DONE);
            }
        } else if (typeHandle.intValue() == 13) { // not yet
            docStatus = Arrays.asList(DocumentStatusEnum.DOING);
            handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("CHO_XU_LY", "CHO_DANH_GIA", "DA_XU_LY_ADD_USER", "THU_HOI_HOAN_THANH");
        } else if (typeHandle.intValue() == 0) { // main type
            handleType = HandleTypeEnum.getListHandleType(HandleTypeEnum.MAIN, HandleTypeEnum.DIRECTION);
            if (statusHandle.intValue() == 0) { // did
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI", "DA_XU_LY_SWITCH");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
            }

            if (statusHandle.intValue() == 1) { // doing - not yet
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("CHO_XU_LY", "DANG_XU_LY",
                        "CHUYEN_DON_VI", "CHO_DANH_GIA", "XIN_DANH_GIA", "DG_CHAP_NHAN", "DG_TU_CHOI", "DA_XU_LY_ADD_USER", "THU_HOI_HOAN_THANH");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.DONE);

                if (returnPreviousNode) {
                    docStatus = new ArrayList<>();
                    docStatus.add(DocumentStatusEnum.RETURN_DOC);
                    docStatus.add(DocumentStatusEnum.DOING);
                    docStatus.add(DocumentStatusEnum.DONE);
                }
            }

            if (statusHandle.intValue() == 2) { // done
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI", "DA_XU_LY_SWITCH", "DA_XU_LY_ADD_USER");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.DONE);
            }

        } else if (typeHandle.intValue() == 1) { // support type
            handleType = HandleTypeEnum.getListHandleType(HandleTypeEnum.SUPPORT);

            if (statusHandle.intValue() == 0) { // did
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
            }

            if (statusHandle.intValue() == 1) { // doing - not yet
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("CHO_XU_LY", "DANG_XU_LY",
                        "CHUYEN_DON_VI", "CHO_DANH_GIA", "XIN_DANH_GIA", "DG_CHAP_NHAN", "DG_TU_CHOI", "DA_XU_LY_ADD_USER", "THU_HOI_HOAN_THANH");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC,
                        DocumentStatusEnum.DONE);
            }

            if (statusHandle.intValue() == 2) { // done
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.DONE);
            }

        } else if (typeHandle.intValue() == 2) { // show type
            handleType = HandleTypeEnum.getListHandleType(HandleTypeEnum.SHOW);

            if (statusHandle.intValue() == 0) { // did
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
            }

            if (statusHandle.intValue() == 1) { // doing - not yet
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("CHO_XU_LY", "DANG_XU_LY",
                        "CHUYEN_DON_VI", "CHO_DANH_GIA", "XIN_DANH_GIA", "DG_CHAP_NHAN", "DG_TU_CHOI", "DA_XU_LY_ADD_USER", "THU_HOI_HOAN_THANH");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC,
                        DocumentStatusEnum.DONE);
            }

            if (statusHandle.intValue() == 2) { // done
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.DONE);
            }
        } else if (typeHandle.intValue() == 3) { // direction type
            handleType = HandleTypeEnum.getListHandleType(HandleTypeEnum.DIRECTION);
            if (statusHandle.intValue() == 0) { // did
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
            }

            if (statusHandle.intValue() == 1) { // doing - not yet
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("CHO_XU_LY", "DANG_XU_LY",
                        "CHUYEN_DON_VI", "CHO_DANH_GIA", "XIN_DANH_GIA", "DG_CHAP_NHAN", "DG_TU_CHOI", "DA_XU_LY_ADD_USER", "THU_HOI_HOAN_THANH");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC,
                        DocumentStatusEnum.DONE);
            }

            if (statusHandle.intValue() == 2) { // done
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.DONE);
            }
        } else if (typeHandle.intValue() == 4) { // văn bản thông luồng type
            handleType = null;
            if (statusHandle.intValue() == 0) { // did
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
            }

            if (statusHandle.intValue() == 1) { // doing - not yet
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("CHO_XU_LY", "DANG_XU_LY", "DA_XU_LY_ADD_USER");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC,
                        DocumentStatusEnum.DONE);
            }

            if (statusHandle.intValue() == 2) { // done
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.DONE);
            }
        } else if (typeHandle == 5) {
            handleType = HandleTypeEnum.getListHandleType(HandleTypeEnum.INTERNAL_INCOMING);
            if (statusHandle.intValue() == 0) { // did
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
            }

            if (statusHandle.intValue() == 1) { // doing - not yet
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("CHO_XU_LY", "DANG_XU_LY",
                        "CHUYEN_DON_VI", "CHO_DANH_GIA", "XIN_DANH_GIA", "DG_CHAP_NHAN", "DG_TU_CHOI", "DA_XU_LY_ADD_USER", "THU_HOI_HOAN_THANH");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC,
                        DocumentStatusEnum.DONE);
            }

            if (statusHandle.intValue() == 2) { // done
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.DONE);
            }
        } else if (typeHandle == 6) {
            handleType = HandleTypeEnum.getListHandleType(HandleTypeEnum.INTERNAL_ISSUED_INCOMING);
            if (statusHandle.intValue() == 0) { // did
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
            }

            if (statusHandle.intValue() == 1) { // doing - not yet
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("CHO_XU_LY", "DANG_XU_LY",
                        "CHUYEN_DON_VI", "CHO_DANH_GIA", "XIN_DANH_GIA", "DG_CHAP_NHAN", "DG_TU_CHOI", "DA_XU_LY_ADD_USER", "THU_HOI_HOAN_THANH");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC,
                        DocumentStatusEnum.DONE);
            }

            if (statusHandle.intValue() == 2) { // done
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.DONE);
            }
        }

        if ("docStatus".equals(rsType)) return (List<T>) docStatus;
        if ("handleStatus".equals(rsType)) return (List<T>) handleStatusList;
        if ("handleType".equals(rsType)) return (List<T>) handleType;
        log.warn("Cannot found result type");
        return new ArrayList<>();
    }

    public static boolean checkCanFinish(Documents doc, DocumentInHandleStatusEnum pStatus) {
        boolean canFinish = true;
        boolean allRetaked = true;
        if (doc.getListChildren().isEmpty() || DocumentStatusEnum.DONE.equals(doc.getStatus())
                || !DocumentInHandleStatusEnum.CHUYEN_DON_VI.equals(pStatus)) {
            canFinish = false;
        } else {
            // update status
            for (Documents child : doc.getListChildren()) {
                if (!DocumentStatusEnum.RETAKE_ORG.equals(child.getStatus())
                        && !DocumentStatusEnum.REJECT_RECEIVE.equals(child.getStatus())) {
                    allRetaked = false;
                    if (!DocumentStatusEnum.DONE.equals(child.getStatus())) {
                        canFinish = false;
                    }
                }
            }
        }
        if (allRetaked) {
            return false;
        }
        return canFinish;
    }

    @Override
    public IRepository<Documents> getRepository() {
        return docRepository;
    }

    @Transactional
    public Documents createDocument(Boolean receive, Documents doc, User u, Long clientId, Long orgReceiveId, Boolean createArrivalNumber, Boolean createResolveTicket) {
        if (!userService.isVanThuVBDen(u)) {
            throw new RestExceptionHandler(Message.NO_CREATE_DOC);
        }

        if (doc.getNumberOrSign() != null && !doc.getNumberOrSign().isEmpty()) {
            List<Long> excludeDocIds = doc.getId() != null ? Collections.singletonList(doc.getId()) : null;
            if (isNumberOrSignExists(doc.getNumberOrSign(),  clientId, excludeDocIds)) {
                throw new RestExceptionHandler(Message.NUMBER_OR_SIGN_EXISTED);
            }
        }

        doc.valids(createArrivalNumber);
        doc.setStatus(doc.getIsComplete() != null && doc.getIsComplete() ? DocumentStatusEnum.DONE : DocumentStatusEnum.NOT_YET);
        doc.setPersonEnterId(u.getId());
        if (orgReceiveId == null) {
            doc.setOrgReceiveId(u.getOrg());
        } else {
            doc.setOrgReceiveId(orgReceiveId);
        }
        if (doc.getPersonSignId() != null) {
            User userSign = userService.findByClientIdAndId(u.getClientId(), doc.getPersonSignId());
            if (userSign == null) {
                throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
            }
            doc.setPersonSign(userSign.getUserName());
        }

        if (doc.getPlaceSendId() != null) {
            Category placeSend = categoryService.findByClientIdAndId(u.getClientId(), doc.getPlaceSendId());
            if (placeSend == null) {
                throw new RestExceptionHandler(Message.NOT_FOUND_CATEGORY);
            }
            doc.setPlaceSend(placeSend.getName());
        }

        // Nơi nhận bên ngoài khác
        addPlaceSendOthers(doc);

        List<Documents> document = this.findByClientIdAndNumberArrivalAndBookId(u.getClientId(),
                doc.getNumberArrival(), doc.getBookId());
        if (!document.isEmpty()) {
            throw new RestExceptionHandler(Message.INVALID_NUMBER_ARRIVAL);
        }

        // update document_book
        if (createArrivalNumber) {
            DocumentBook db = dbService.getOne(doc.getBookId());
            if (db == null) {
                throw new RestExceptionHandler(Message.DOCUMENT_BOOK_NOT_FOUND);
            }
            db.setCurrentNumber(doc.getNumberArrival());
            dbService.save(db);

            // save number arrival str
            doc.setNumberArrivalStr(db.getNumberOrSign() != null ? doc.getNumberArrival() + db.getNumberOrSign()
                    : doc.getNumberArrival().toString());
        }

        if (doc.getDateIssued() == null) {
            doc.setDateIssued(new Date());
        }
        Long lastNodeId = null;
        if (doc.getIsComplete() != null && doc.getIsComplete()) {
            lastNodeId = bpmnService2.getLastNodeByOrgId(u.getOrg(), true);

            doc.setNode(lastNodeId);
            doc.setPreNode(Constant.START_NODE);
        }

        // save doc
        try {
            doc.setDocBaseModel(u);
            this.save(doc);
        } catch (Exception e) {
            throw new RestExceptionHandler(Message.ERROR_SYS);
        }

        if (Boolean.TRUE.equals(receive)) {
            // save tracking
            trackingServices.save(doc, DocumentInTrackingEnum.RECEIVE, u.getId(), clientId);

            // save processs
            DocumentInProcess f = processService.findByDocIdAndFirstStep(doc.getId());
            if (f == null) {
                processService.save(Constant.START_NODE, doc.getId(), DocumentInHandleStatusEnum.MOI_NHAN,
                        HandleTypeEnum.MAIN, Constant.START_STEP, Constant.START_STEP, u, null, u.getId(),
                        doc.getDeadline(), null);
            } else {
                f.setToUser(u.getId());
                f.setHandleStatus(DocumentInHandleStatusEnum.MOI_NHAN);
                processService.save(f);
            }

            // Delete notification
            noticeService.setActiveByDocIdAndDocType(doc.getId(), DocumentTypeEnum.VAN_BAN_DEN, false);

            if (doc.getAutoCreateSeal()) {
                List<Attachment> attachments = attService.findByObjIdCreateDateDesc(doc.getId());
                if (!attachments.isEmpty()) {
                    Attachment firstAttachment = attachments.get(0);
                    try {
                        if (firstAttachment.getType().equals("application/pdf")) {
                            Path inputFilePath = fileService.getPath(firstAttachment.getName());
                            String filenameParsed = FilesStorageService.parse(String.format("%s.pdf", doc.getNumberArrivalStr()));
                            Path outputFilePath = fileService.getPath(filenameParsed);
                            pdfService.addWatermarkToPDF(inputFilePath.toString(), outputFilePath.toString(), INCOMING_STRING_LITERAL, doc.getNumberArrivalStr(), doc.getDateIssued());

                            Attachment clonedAttachment = firstAttachment.clone();
                            clonedAttachment.setId(null);
                            clonedAttachment.setName(filenameParsed);

                            attService.save(clonedAttachment);
                            attService.getRepository().delete(firstAttachment);
                        }
                    } catch (Exception e) {
                        e.printStackTrace();
                    }
                }
            }
        } else {
            // save tracking
            trackingServices.save(doc, DocumentInTrackingEnum.CREATE, u.getId(), clientId);
            // save processs
            processService.save(Constant.START_NODE, doc.getId(), DocumentInHandleStatusEnum.MOI_TAO, HandleTypeEnum.MAIN, Constant.START_STEP, Constant.START_STEP, u, null,
                    u.getId(), doc.getDeadline(), null, u, clientId);
        }
        // save strace
        straceService.save(doc.getId(), ActionEnum.ADD.getName(), doc.getPreview(), doc.getIdCat(), u, clientId);

        // Hoàn thành văn bản
        if (doc.getIsComplete() != null && doc.getIsComplete()) {
            trackingServices.save(doc.getId(), DocumentInTrackingEnum.FINISH, u.getId());
            if (lastNodeId != null) {
                processService.save(lastNodeId, doc.getId(), DocumentInHandleStatusEnum.DA_XU_LY, HandleTypeEnum.MAIN, Constant.START_STEP, Constant.START_STEP, u, null,
                        u.getId(), doc.getDeadline(), null);
            }
            straceService.save(doc.getId(), ActionEnum.DONE.getName(), doc.getPreview(), doc.getIdCat(), u, clientId);
        }

        //save file resolved
        try {
            if (isPhieuTrinh && receive) this.saveResolveFile(doc);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return doc;
    }

    public ListObjectDto<Documents> findReceive(Boolean expired, Boolean important, String text, String numberOrSign,
                                                String preview, Long docType, Long docFields, Long docStatusReceipt, Pageable page) {
        User u = BussinessCommon.getUser();
        if (!userService.isVanThuVBDen(u)) {
            throw new RestExceptionHandler(Message.ROLE_LIBRARIAN);
        }

        List<Long> userList = userService.getListIdsVanThuVBDenByOrg(u.getOrg());
        List<DocumentStatusEnum> status = Arrays.asList(DocumentStatusEnum.NOT_YET, DocumentStatusEnum.RETURN_DOC);
        if (returnPreviousNode) {
            status = Arrays.asList(DocumentStatusEnum.NOT_YET);
        }

        Date now = DateTimeUtils.handleSubmit(new Date());

        if (!clericalOrg && userList.isEmpty()) {
            return new ListObjectDto<>();
        }

        Page<Documents> rsPage = docRepository.findReceiveAdvance(text, clericalOrg, expired, now, important,
                DocumentTypeEnum.VAN_BAN_DEN, u.getId(), userList, numberOrSign, preview, docType, docFields, status,
                docStatusReceipt, u.getClientId(), page);

        if (!BussinessCommon.isEmptyPage(rsPage)) {
            List<Documents> rsList = rsPage.getContent();
            List<Long> docIdList = rsList.stream().filter(i -> i.getStatus().equals(DocumentStatusEnum.RETURN_DOC))
                    .map(Documents::getId).collect(Collectors.toList());
            if (!BussinessCommon.isEmptyList(docIdList)) {
                List<DocumentComment> cmtList = cmtService.getListByListDocId(docIdList);
                rsList.forEach(dto -> {
                    Optional<DocumentComment> cmt = cmtList.stream().filter(j -> j.getDocId().equals(dto.getId()))
                            .findFirst();
                    if (cmt.isPresent()) {
                        dto.setReason(cmt.get().getComment());
                    }
                });
            }
            List<Long> idList = rsPage.getContent().stream().map(Documents::getId).collect(Collectors.toList());
            List<DocumentUser> listDU = docUserService.findByDocTypeAndDocIdInAndUserId(DocumentTypeEnum.VAN_BAN_DEN,
                    idList, u.getId());
            Map<Long, DocumentUser> mapPOfDU = new HashMap<>();
            for (DocumentUser du : listDU) {
                mapPOfDU.put(du.getDocId(), du);
            }
            rsList.forEach(dto -> {
                if (mapPOfDU.get(dto.getId()) != null) {
                    dto.setImportant(mapPOfDU.get(dto.getId()).getImportant());
                } else {
                    dto.setImportant(false);
                }

                dto.hideDataByConfidentialDoc();
            });
        }

        return BussinessCommon.paging(rsPage);
    }

    public Documents findByClientIdAndId(Long idDoc) {
        return docRepository.findByClientIdAndId(BussinessCommon.getClientId(), idDoc);
    }

    private boolean canRetake(Documents doc) {
        if (DocumentStatusEnum.NOT_YET.equals(doc.getStatus()) || DocumentStatusEnum.RETAKE_DOC.equals(doc.getStatus())) {
            return false;
        }
        return true;
    }

    public DocumentDetail findDocumentDetailByClientIdAndId(Long docId) {
        User user = BussinessCommon.getUser();
        Documents doc = docRepository.findByClientIdAndId(user.getClientId(), docId);
        DocumentDetail documentDetail = new DocumentDetail();
        DocumentInProcess oProcess = processService.findByToUserAndDocId(user.getId(), docId);
        if (doc != null) {

            List<Attachment> atms = doc.getAttachments();
            doc.setAttachments(getAttachmentsByParent(doc.getParent(), atms));
            if (doc.getListChildren() != null && !doc.getListChildren().isEmpty()) {
                List<DocumentBasicDto> listChildren = new ArrayList<>();
                for (Documents child : doc.getListChildren()) {
                    if (!DocumentStatusEnum.RETAKE_ORG.equals(child.getStatus())
                            && !DocumentStatusEnum.REJECT_RECEIVE.equals(child.getStatus())) {
                        listChildren.add(new DocumentBasicDto(child));
                    }
                }
                documentDetail.setListChildrenDoc(listChildren);
            }
            if (doc.getParentId() != null) {
                documentDetail.setParentDoc(new DocumentBasicDto(doc.getParent()));
            }

            if (doc.getListResponseDoc() != null) {
                List<DocumentBasicDto> listResponse = new ArrayList<>();
                for (DocumentInOut child : doc.getListResponseDoc()) {
                    listResponse.add(new DocumentBasicDto(child.getDocOut()));
                }
                documentDetail.setListResponseDoc(listResponse);
            }
            if (oProcess != null && oProcess.getDeadline() != null) {
                doc.setDeadline(oProcess.getDeadline());
            } else {
                doc.setDeadline(null);
            }
            documentDetail.setListTask(taskService.getListTaskDtoByDocIdAndDocType(docId, true));

            List<Long> ids = new ArrayList<>();
            ids.add(doc.getBookId());
            ids.add(doc.getDocTypeId());
            ids.add(doc.getUrgentId());
            ids.add(doc.getSecurityId());
            ids.add(doc.getDocFieldsId());
            ids.add(doc.getMethodReceiptId());

            List<Category> categories = categoryService.findAllById(ids);
            Optional<Category> optionalCategory;

            documentDetail.setBookName(doc.getDocumentBook() != null ? doc.getDocumentBook().getName() : "");
            optionalCategory = categories.stream().filter(category -> category.getId().equals(doc.getDocTypeId()))
                    .findFirst();
            documentDetail.setDocTypeName(optionalCategory.isPresent() ? optionalCategory.get().getName() : "");

            optionalCategory = categories.stream().filter(category -> category.getId().equals(doc.getDocFieldsId()))
                    .findFirst();
            documentDetail.setDocFieldsName(optionalCategory.isPresent() ? optionalCategory.get().getName() : "");

            optionalCategory = categories.stream().filter(category -> category.getId().equals(doc.getUrgentId()))
                    .findFirst();
            documentDetail.setUrgentName(optionalCategory.isPresent() ? optionalCategory.get().getName() : "");

            optionalCategory = categories.stream().filter(category -> category.getId().equals(doc.getSecurityId()))
                    .findFirst();
            documentDetail.setSecurityName(optionalCategory.isPresent() ? optionalCategory.get().getName() : "");

            documentDetail.setDocStatusName(doc.getStatus() != null ? doc.getStatus().getName() : "");

            optionalCategory = categories.stream().filter(category -> category.getId().equals(doc.getMethodReceiptId()))
                    .findFirst();
            documentDetail.setMethodReceiptName(optionalCategory.isPresent() ? optionalCategory.get().getName() : "");
            // Attachment of parent doc - For case org transfer
//			Documents parent = doc.getParent();
//			if (parent != null && !BussinessCommon.isEmptyList(parent.getAttachments())) {
//				atms.addAll(parent.getAttachments());
//			}

            DocumentDetailDto detailDto = setDocumentDetailDto(doc);
            documentDetail.setDocument(detailDto);
        } else {
            throw new RestExceptionHandler(Message.NOT_FOUND_DOC);
        }
        return documentDetail;
    }

    private DocumentDetailDto setDocumentDetailDto(Documents document) {
        DocumentDetailDto documentDetailDto = new DocumentDetailDto();
        DocumentOut documentOut = null;
        if (document.getDocOutId() != null) {
            documentOut = docOutService.findByDocId(document.getDocOutId());
            List<DocumentReceive> documentReceives = documentReceiveService.findByClientIdAndDocId(BussinessCommon.getClientId(), documentOut.getId());
            List<DocumentReceive> documentReceivesUpdated = documentReceives.stream().peek(dr -> {
                if (dr.getType().equals("ORG")) {
                    Optional<Organization> organizationOptional = organizationService.findById(dr.getReceiveId());
                    if (organizationOptional.isPresent()) {
                        dr.setOrgName(organizationOptional.get().getName());
                        dr.setName(organizationOptional.get().getName());
                    }
                } else {
                    Optional<User> userOptional = userService.findById(dr.getReceiveId());
                    if (userOptional.isPresent()) {
                        dr.setFullName(userOptional.get().getFullName());
                        dr.setName(userOptional.get().getFullName());
                        dr.setPositionName(userOptional.get().getPositionModel().getName());
                    }
                }
            }).collect(Collectors.toList());
            documentOut.setListReceive(documentReceivesUpdated);
            if (documentOut.getIsInternalDocument() != null && !documentOut.getIsInternalDocument()) {
                documentOut = null;
            }
        }
        documentDetailDto.setFromDocuments(document, documentOut);
        return documentDetailDto;
    }

    public Documents validDocId(Long docId) {
        if (docId == null) {
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);
        }
        Optional<Documents> doc = findById(docId);
        if (!doc.isPresent()) {
            throw new RestExceptionHandler(Message.NOT_FOUND_DOC);
        }
        return doc.get();
    }

    public Documents deleteDoc(Documents doc) {
        doc.setActive(false);
        getRepository().save(doc);
        // update document_book
        DocumentBook db = dbService.findByBookId(doc.getBookId());
        if (db != null) {
            Long maxArrival = docRepository.getMaxNumberArrivalOfDocumentByBookId(BussinessCommon.getClientId(), doc.getBookId());
            db.setCurrentNumber(maxArrival);
            dbService.save(db);
        }
        return doc;
    }

    @Transactional
    public void returnDoc(Long docId, String comment, MultipartFile[] files, boolean isDelegate) {
        User u = BussinessCommon.getUser();
        DocumentInProcess oProcess = null;
        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);
        // update old process is DA_TRA_LAI_UQ
        boolean isAddForDelegate = false;

        if (isDelegate) {
            oProcess = processService.findByDelegateAndDocId(u.getId(), docId, HandleTypeEnum.MAIN);
            if (oProcess == null || !processService.isType(oProcess.getHandleType(), HandleTypeEnum.MAIN)
                    || !processService.isStatus(oProcess.getHandleStatus(), processService.HANDLE_STATUS_NOT_YET)) {
                throw new RestExceptionHandler(Message.NO_DELEGATE);
            }
            isAddForDelegate = true;
        } else {
            oProcess = processService.findByToUserAndDocId(u.getId(), docId);
            if (oProcess == null || !processService.isType(oProcess.getHandleType(), HandleTypeEnum.MAIN)
                    || !processService.isStatus(oProcess.getHandleStatus(), processService.HANDLE_STATUS_NOT_YET)) {
                throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
            }
        }

        DocumentInProcess fisrtStep = processService.findByDocIdAndFirstStep(docId);
        if (fisrtStep == null) {
            throw new RestExceptionHandler(Message.NO_RETURN_DOC);
        }

        // update node and status
        Documents doc = processService.updateDocByStatus(docId, DocumentStatusEnum.RETURN_DOC);

        // add for transfer
        processService.save(Constant.START_NODE, docId, DocumentInHandleStatusEnum.CHO_XU_LY, HandleTypeEnum.MAIN,
                oProcess.getStep() + 1, 0, fisrtStep.getToUsers(), null, null, null, null);
        trackingServices.save(docId, DocumentInTrackingEnum.INCOMING, fisrtStep.getToUser());

        // update for old process
        List<DocumentInProcess> prList = processService.update(docId, oProcess.getStep(),
                DocumentInHandleStatusEnum.DA_XU_LY, u.getId(), Constant.RETURN_DOC_TYPE);
        List<Long> userId = prList.stream().map(DocumentInProcess::getToUser).collect(Collectors.toList());
        List<User> userIds = userService.findByIds(userId, true);
        trackingServices.save(doc, DocumentInTrackingEnum.FINISH, userIds, u.getId(), Constant.RETURN_DOC_TYPE);

        // add for new process is DA_TRA_LAI_UQ
        if (isAddForDelegate) {
            processService.saveFrDelegate(oProcess, HandleTypeEnum.MAIN, DocumentInHandleStatusEnum.DA_TRA_LAI_UQ);
            trackingServices.save(docId, DocumentInTrackingEnum.REJECT_UQ, u.getId());
        }

        // comment
        saveCommentAndAttach(docId, files, comment);

        // notification
        notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);
        notiService.add(fisrtStep.getFrUser(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                NotificationHandleStatusEnum.BI_TRA_LAI, ModuleCodeEnum.DOC_OUT_LIST);
        if (!fisrtStep.getCreateBy().equals(fisrtStep.getFrUser())) {
            notiService.add(fisrtStep.getCreateBy(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                    NotificationHandleStatusEnum.BI_TRA_LAI, ModuleCodeEnum.DOC_OUT_LIST);
        }
    }

    @Transactional
    public boolean retakeByStep(Long docId, String comment, MultipartFile[] files, boolean isDelegate) {
        User u = BussinessCommon.getUser();
        Documents doc = validDocId(docId);
        //Check trạng thái văn bản
        if (!DocumentStatusEnum.DOING.equals(doc.getStatus())) {
            throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
        }
        List<DocumentInProcess> listLastProcess = processService.findByDocIdAndLastedStep(Arrays.asList(docId));
        if (listLastProcess.isEmpty()) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        boolean canRetake = true;
        for (DocumentInProcess p : listLastProcess) {
            if (DocumentInHandleStatusEnum.CHUYEN_DON_VI.equals(p.getHandleStatus())) {
                canRetake = false;
                break;
            }
        }
        if (!canRetake) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        DocumentInProcess currUserProcess = null;
        DocumentInProcess delegateProcess = null;
        List<DocumentInProcess> previousProcessList = processService.getTransferStep(docId);
        if (!previousProcessList.isEmpty()) {
            for (DocumentInProcess dip : previousProcessList) {
                if (dip.getToUser().equals(u.getId()) || dip.getDelegaterId() != null && dip.getDelegaterId().equals(u.getId())) {
                    currUserProcess = dip;
                    break;
                }
            }
        }
        if (!isDelegate) {
            //currUserProcess = processService.findByDocIdAndStepAndHandleTypeAndToUser(docId, listLastProcess.get(0).getStep() - 1, u.getId(), HandleTypeEnum.MAIN);
        } else {
            //currUserProcess = processService.findByDocIdAndStepAndHandleTypeAndDelegaterId(docId, listLastProcess.get(0).getStep() - 1, u.getId(), HandleTypeEnum.MAIN);
            delegateProcess = processService.findByDocIdAndStepAndHandleTypeAndToUser(docId, currUserProcess.getStep(), u.getId(), HandleTypeEnum.MAIN);
        }
        if (currUserProcess == null) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        if (delegateProcess != null) {
            delegateProcess.setActive(false);
            processService.save(delegateProcess);
        }
        //Deactive list last process
        List<Long> listUsers = new ArrayList<>();
        for (DocumentInProcess dip : listLastProcess) {
            listUsers.add(dip.getToUser());
            if (dip.getDelegaterId() != null) {
                listUsers.add(dip.getDelegaterId());
            }
            dip.setActive(false);
        }
        processService.saveAll(listLastProcess);
        //Update process
        currUserProcess.setHandleStatus(DocumentInHandleStatusEnum.DANG_XU_LY);
        currUserProcess.setProgress(0);
        processService.save(currUserProcess);
        //Update node
        if (currUserProcess.getStep().equals(1)) {
            doc.setNode(null);
            doc.setStatus(DocumentStatusEnum.NOT_YET);
        } else {
            if (currUserProcess.getNode().equals(0L)) {
                doc.setNode(null);
                doc.setStatus(DocumentStatusEnum.NOT_YET);
            } else {
                doc.setNode(currUserProcess.getNode());
                if (DocumentStatusEnum.RETURN_DOC.equals(doc.getStatus())) {
                    doc.setStatus(DocumentStatusEnum.DOING);
                }
            }
        }
        docRepository.save(doc);
        //Delete notification
        notiService.setActiveByListUserIdAndDocIdAndDocType(listUsers, docId, DocumentTypeEnum.VAN_BAN_DEN, false);
        //Add tracking
        if (!isDelegate) {
            trackingServices.save(docId, DocumentInTrackingEnum.RETAKE, u.getId());
        } else {
            trackingServices.save(docId, DocumentInTrackingEnum.RETAKE_UQ, u.getId());
        }
        // comment
        saveCommentAndAttach(docId, files, comment);
        return true;
    }

    public List<CategoryInitDto> getMaxNumberArrival() {
        List<CategoryInitDto> list;
        if (orgConfig) {
            List<Long> clericalOrgs = new ArrayList<>();
            if (clericalOrg) {
                clericalOrgs.addAll(clericalOrgService.getClericalOrg(BussinessCommon.getUserId()));
            } else {
                clericalOrgs.add(BussinessCommon.getOrgId());
            }
            list = docRepository.getMaxNumberArrivalByOrgIdAndClientId(clericalOrgs, BussinessCommon.getClientId());
        } else {
            list = docRepository.getMaxNumberArrivalByClientId(BussinessCommon.getClientId());
        }
        list.forEach(i -> {
            if (i.getValue() == null) {
                i.setValue(0L);
            }
            if (StringUtils.isNullOrEmpty(i.getNumberOrSign())) {
                i.setNumberOrSign("");
            }
        });
        return list;
    }

    public List<Documents> findByClientIdAndNumberArrivalAndBookId(Long clientId, Long numberArrival, Long bookId) {
        return docRepository.findByClientIdAndNumberArrivalAndBookIdAndActiveTrue(clientId, numberArrival, bookId);
    }

    public ListObjectDto<DocumentReplyDto> findDocReply(String numberOrSign, String preview, String orgIssuedNameL,
                                                        DocumentStatusEnum docStatusId, Date startArrival, Date endArrival, Date startIssued, Date endIssued,
                                                        Pageable page) {
        User u = BussinessCommon.getUser();
        List<DocumentInHandleStatusEnum> handleStatusList = DocumentInHandleStatusEnum
                .getHandleStatusByName("DANG_XU_LY", "CHO_XU_LY", "DA_XU_LY", "XIN_DANH_GIA", "DG_CHAP_NHAN", "DG_TU_CHOI", "CHO_DANH_GIA", "DA_XU_LY_UQ", "CHUYEN_DON_VI", "CHUYEN_DON_VI_UQ", "DA_XU_LY_SWITCH", "DA_XU_LY_ADD_USER");
        List<DocumentStatusEnum> hideDocStatus = Arrays.asList(DocumentStatusEnum.RETAKE_DOC, DocumentStatusEnum.REJECT_RECEIVE);
        Page<Documents> dPage = docRepository.findDocReplys(hideDocStatus, handleStatusList, u.getId(),
                u.getClientId(), numberOrSign, preview, StringUtils.decodeFromUrl(orgIssuedNameL), docStatusId,
                startArrival, endArrival, startIssued, endIssued, page);
        if (BussinessCommon.isEmptyPage(dPage)) {
            return BussinessCommon.paging(null);
        }
        List<DocumentReplyDto> dtoList = new ArrayList<>();
        List<Documents> dList = dPage.getContent();
        int no = 0;
        for (Documents d : dList) {
            DocumentReplyDto dto = new DocumentReplyDto(d);
            dto.setNo(++no);
            dtoList.add(dto);
        }
        return BussinessCommon.paging(new PageImpl<>(dtoList, page, dPage.getTotalElements()));
    }

    public List<DocumentReplyDto> getListDocDtoByIds(Long[] ids) {
        if (ids == null) {
            return null;
        }
        int no = 0;
        List<DocumentReplyDto> rsList = docRepository.findDocReplyByIds(ids, BussinessCommon.getClientId());
        if (rsList == null) {
            return null;
        }
        for (DocumentReplyDto dto : rsList) {
            dto.setNo(++no);
        }
        return rsList;
    }

    private List<Long> getPId(List<Long> userIds) {
        List<KnowableDto> docIds = docRepository.getPIdByMaxStep(userIds, BussinessCommon.getClientId());
        return docIds.stream().map(KnowableDto::getId).collect(Collectors.toList());
    }

    public ListObjectDto<DocumentDto> findProcessingDoc(Integer dayLeft, Integer typeHandle, Boolean expired, Boolean important,
                                                        Integer statusHandle, String text, String numberOrSign, String preview, Long docType, Long docFields, Long posId, Pageable page) {
        User u = BussinessCommon.getUser();
        boolean vanthu = userService.isVanThuVBDen(u);
        Page<DocumentDto> rsPage = null;
        List<Long> userList = new ArrayList<>();
        userList.add(u.getId());
        Integer endTask = null;
        if (statusHandle != null && statusHandle.intValue() == 2) { // Done tab
            endTask = 2;
        } else if (statusHandle != null && statusHandle.intValue() == 0) { // Did tab
            endTask = 0;
        }
        List<DocumentStatusEnum> docStatus = getObjStatusByType(typeHandle, statusHandle, vanthu, returnPreviousNode, "docStatus");
        List<DocumentInHandleStatusEnum> handleStatusList = getObjStatusByType(typeHandle, statusHandle, vanthu, returnPreviousNode, "handleStatus");
        List<HandleTypeEnum> handleType = getObjStatusByType(typeHandle, statusHandle, vanthu, returnPreviousNode, "handleType");
        Date now = DateTimeUtils.handleSubmit(new Date());
        Boolean mergedLines = typeHandle.intValue() == 4 ? Boolean.TRUE : null;
        List<Long> userIds = new ArrayList<>();
        Category positionVanThuBan = categoryService.findByName(u.getClientId(), Constant.VAN_THU_MAIN);
        if (positionVanThuBan != null && u.getPositionModel() != null && (u.getPositionModel().getId().longValue() == positionVanThuBan.getId().longValue())) {
            userIds = userRepository.getUsersByNameCategory(Constant.VAN_THU_MAIN, u.getOrg());
            if (userIds.isEmpty()) {
                throw new RestExceptionHandler(Message.EMPTY_CLERICAL_HANDLE);
            }
        } else {
            userIds.add(u.getId());
        }
        List<Long> maxStepByDocs = getPId(userIds);
        rsPage = docRepository.findByMultiConditions(endTask, mergedLines, text, dayLeft, expired, now, important, DocumentTypeEnum.VAN_BAN_DEN,
                userIds, docStatus, handleStatusList, handleType, numberOrSign, preview,
                docType, docFields, maxStepByDocs, BussinessCommon.getClientId(), posId, page);

        if (!BussinessCommon.isEmptyPage(rsPage)) {
            List<DocumentDto> rsList = rsPage.getContent();
            for (DocumentDto d : rsList) {
                if (d.getDoc() != null && d.getDoc().getDocTypeId() != null) {
                    Category docTypeCategory = getTmpCategoryById(d.getDoc().getDocTypeId());
                    if (docTypeCategory != null) {
                        d.setDocTypeName(docTypeCategory.getName());
                    }
                }
            }
            tmpCategoryMap.clear();
            Long org = u.getOrg();
            List<Long> setOrg = orgService.listParentOrg(org);
            Set<Long> setNode = new HashSet<>();
            Map<Long, Long> doc2Node = new HashMap<>();
            List<Long> nodeIds = rsList.stream().map(DocumentDto::getProcessNode).distinct().collect(Collectors.toList());
            List<Long> docIdList = rsList.stream().map(DocumentDto::getDoc).map(Documents::getId)
                    .collect(Collectors.toList());
            TYPE_DOCUMENT typeDocument = TYPE_DOCUMENT.INCOMING;
            if (typeHandle == 5) {
                typeDocument = TYPE_DOCUMENT.INTERNAL_INCOMING;
            } else if (typeHandle == 6) {
                typeDocument = TYPE_DOCUMENT.INTERNAL_ISSUED_INCOMING;
            }

            Map<Long, List<NodeDto>> nextNodeIds = bpmnService.getNextNodes(nodeIds, typeDocument);
            Map<Long, List<NodeDto>> dataByNodeIds = bpmnService.getDataByNodeId(nodeIds);
            Map<Long, Boolean> hasAskToUsers = manipulationService.hasAskToUser(docIdList, u.getId());
            Map<Long, DocumentInProcess> pMaps = processService.getLastStepByDocId(docIdList);
            Boolean leaderUnit = authorityService.isUserHasAuthority(BussinessCommon.getUserId(), null, AuthorityEnum.LEADERSHIP_UNIT);
            for (DocumentDto dto : rsList) {
                Documents doc = dto.getDoc();
                if (doc == null) {
                    continue;
                }
                if (dto.getNode() != null && dto.getNode() != 0) {
                    NodeModel2 nodeModel2 = nodeRepository2.getByIdAndClientId(dto.getNode(), BussinessCommon.getClientId());
                    if (nodeModel2 != null) {
                        dto.setBpmnId(nodeModel2.getBpmnId());
                        dto.setBpmnName(nodeModel2.getBpmn().getName());
                    }
                }
                doc2Node.put(doc.getId(), dto.getProcessNode());
                setNode.add(dto.getProcessNode());
                setBtn(dto, nextNodeIds, dataByNodeIds, hasAskToUsers, leaderUnit, pMaps);
            }

            Set<Long> setNodeValid = nodeRepo.validNode(setNode, org, u.getPosition(), u.getId(), setOrg);
            List<CategoryDto> mapHandle = docRepository.findNextHandle(docIdList, u.getClientId());
            List<Long> docIds = rsList.stream().map(DocumentDto::getDocId).distinct().collect(Collectors.toList());
            List<ObjectRead> objectReads = objectReadService.findAllByObjIdInAndUserIdInAndTypeAndClientIdAndActive(
                    docIds, userIds, DocumentTypeEnum.VAN_BAN_DEN);

            rsList.forEach(dto -> {
                boolean read = objectReads.stream()
                        .anyMatch(it -> it.getObjId() != null && it.getObjId().equals(dto.getDocId()));
                dto.setRead(read);
                Long docId = dto.getDoc().getId();
                Long nodeId = doc2Node.get(docId);
                dto.getButton().setAllowConfig(setNodeValid.contains(nodeId)); //gia hạn xử lý
                Optional<CategoryDto> o = mapHandle.stream().filter(i -> dto.getDoc().getId().equals(i.getId()))
                        .findFirst();
                o.ifPresent(categoryDto -> dto.setNextHandle(categoryDto.getName()));
            });
        }

        return BussinessCommon.paging(rsPage);
    }

    private void setBtn(DocumentDto docDto, Map<Long, List<NodeDto>> nextNodeIds,
                        Map<Long, List<NodeDto>> dataByNodeIds, Map<Long, Boolean> hasAskToUsers, Boolean leaderUnit, Map<Long, DocumentInProcess> pMaps) {
        Documents doc = docDto.getDoc();
        ButtonDto dto = new ButtonDto();
        boolean lastNode = false;
        boolean isDoneDoc = DocumentStatusEnum.DONE.equals(doc.getStatus());
        boolean bpmnError = false;
        List<NodeDto> pNodes = new ArrayList<>();
        Long key = docDto.getProcessNode();
        if (nextNodeIds.containsKey(key)) {
            pNodes = nextNodeIds.get(key);
            bpmnError = processService.bpmnError(pNodes);
            lastNode = processService.lastNode(pNodes);
        }

        dto.setBpmnError(bpmnError);
        DocumentInProcess old = docDto.getP();
        if (hasAskToUsers.containsKey(doc.getId())) {
            dto.setCanReply(hasAskToUsers.get(doc.getId()));
        }
        boolean canFinish = checkCanFinish(doc, old.getHandleStatus());
        dto.setCanFinish(canFinish);
        dto.setCanAsk(processService.canAsk(old, isDoneDoc));
        dto.setCanRetake(processService.canRetake(old));

        Boolean canRqReview = false;
        Boolean canReview = false;
        List<NodeDto> reviewNodes = new ArrayList<>();
        List<NodeDto> closeBranchNodes = new ArrayList<>();
        if (dataByNodeIds.containsKey(key)) {
            reviewNodes = dataByNodeIds.get(key);
            closeBranchNodes = dataByNodeIds.get(key);
        }
        if (DocumentInHandleStatusEnum.CHO_DANH_GIA.equals(old.getHandleStatus())) {
            canReview = true;
            dto.setCanReview(canReview);
        } else {
            boolean tmpReview = false;
            tmpReview = processService.review(reviewNodes);

            canRqReview = processService.canRequestReview(old, tmpReview);
            dto.setCanRequestReview(canRqReview);
        }
        boolean isCloseBranchNodes = processService.isCloseBranchNodes(closeBranchNodes);
        dto.setCanTransfer(processService.canTransfer(old, pNodes, canRqReview || canReview));
        dto.setCanOrgTransfer(processService.canOrgTransfer(old, pNodes, canRqReview || canReview, isDoneDoc));
        dto.setCanReturn(processService.canReturn(old, isDoneDoc));
        dto.setCanDone(processService.canDone(old, canRqReview || canReview, lastNode, isDoneDoc, doc.getMergedLines(), isCloseBranchNodes));
        dto.setCanRetakeDone(!dto.isCanDone() && processService.canRetakeDone(old, doc));
        dto.setCanSwitchOrAdd(processService.canSwitchOrAdd(old));
        dto.setCanRead(processService.canRead(old, doc));
        docDto.setButton(dto);
    }

    public List<Long> getListUserId(boolean isLibrarian) {
        User u = BussinessCommon.getUser();
        List<Long> arr = new ArrayList<>();
        if (isLibrarian) {
            arr = userService.getListIdsVanThuVBDenByOrg(u.getOrg());
        }
        arr.add(u.getId());
        return arr;
    }

    public ListObjectDto<DocumentDto> findAll(Boolean expired, Boolean important, String text, String userExe,
                                              String numberOrSignL, String previewL, Long bookIdL, Long docTypeL, String orgIssuedNameL,
                                              Long docFieldsIdL, DocumentStatusEnum docStatusIdL, Long urgentIdL, Long securityIdL, String numberArrivalL,
                                              String orgExeL, Date startArrivalL, Date endArrivalL, Date startIssuedL, Date endIssuedL,
                                              Date startReceived, Date endReceived, String personSignL, HandleTypeEnum handleType,
                                              DocumentInHandleStatusEnum handleStatus, Pageable page) {
        User u = BussinessCommon.getUser();
        boolean isLibrarian = userService.isVanThuVBDen(u);
        Date now = DateTimeUtils.handleSubmit(new Date());
        Boolean delegateDoc = null;
        if (DocumentStatusEnum.DELEGATE_DOC.equals(docStatusIdL)) {
            delegateDoc = true;
            docStatusIdL = null;
        }

        Integer endTask = null;
        if (DocumentInHandleStatusEnum.HOAN_THANH.equals(handleStatus)) { // Done tab
            endTask = 2;
            handleStatus = null;
        } else if (DocumentInHandleStatusEnum.DA_XU_LY.equals(handleStatus)) { // Did tab
            endTask = 0;
        }

        List<Long> witDocument = getWithDocument(BussinessCommon.getClientId(), clericalOrg, u.getId(), isLibrarian);
        List<Long> withProcess = getWithProcess(delegateDoc, StringUtils.decodeFromUrl(orgExeL), userExe, handleType, handleStatus, endTask);
        Page<DocumentDto> t = docRepository.findAdvanceAll(text, expired, now, important, getUserOrListVanThuBan(), numberOrSignL,
                StringUtils.decodeFromUrl(previewL), bookIdL, docTypeL, StringUtils.decodeFromUrl(orgIssuedNameL),
                docFieldsIdL, docStatusIdL, urgentIdL, securityIdL, numberArrivalL, startArrivalL, endArrivalL,
                startIssuedL, endIssuedL, startReceived, endReceived, StringUtils.decodeFromUrl(personSignL),
                BussinessCommon.getClientId(), withProcess, witDocument, page);
        if (!BussinessCommon.isEmptyPage(t)) {
            setData(t.getContent());
        }
        return BussinessCommon.paging(t);
    }

    private List<Long> getWithProcess(Boolean delegateDoc, String orgExe, String userExe, HandleTypeEnum handleType,
                                      DocumentInHandleStatusEnum handleStatus, Integer endTask) {
        return docRepository.getWithProcess(delegateDoc, orgExe, userExe, handleType, handleStatus, endTask,
                BussinessCommon.getUserId(), BussinessCommon.getClientId());
    }

    private List<Long> getWithDocument(Long clientId, boolean clericalOrg, Long userId, Boolean isLibrarian) {
        if (Boolean.TRUE.equals(isLibrarian)) {
            return new ArrayList<Long>();
        }
        return docRepository.witDocument(clientId, clericalOrg, userId);
    }

    /**
     * #2765 Đơn vị XL (tra cứu tìm kiếm ) : cần show đơn vị cuối cùng xử lý
     *
     * @param content
     */
    private void setDataOld(List<DocumentDto> content) {
        List<Long> docIds = content.stream().map(DocumentDto::getDocId).collect(Collectors.toList());
        List<IdName> orgNames = docRepository.getOrgMappingDocId(docIds, BussinessCommon.getClientId());
        Map<Long, String> map = new HashMap<>();
        orgNames.forEach(i -> {
            if (!map.containsKey(i.getId())) {
                map.put(i.getId(), i.getName());
            }
        });

        content.forEach(i -> {
            Long key = i.getDocId();
            if (map.containsKey(key)) {
                i.setOrgExe(map.get(key));
            }
        });
    }

    private void setData(List<DocumentDto> content) {
        List<Long> docIds = content.stream().map(DocumentDto::getDocId).collect(Collectors.toList());
        List<IdName> orgNames = docRepository.getOrgMappingDocId(docIds, BussinessCommon.getClientId());
        for (DocumentDto documentDto : content) {
            if (documentDto.getDoc().getListChildren() != null && documentDto.getDoc().getListChildren().size() > 0) {
                for (Documents documents : documentDto.getDoc().getListChildren()) {
                    if (documents.getOrgReceive() != null && documents.getStatus() != DocumentStatusEnum.RETAKE_ORG) {
                        documentDto.setOrgExe((documentDto.getOrgExe() == null || documentDto.getOrgExe().equals("")) ? documents.getOrgReceive().getName() : documentDto.getOrgExe() + ", " + documents.getOrgReceive().getName());
                    }
                    if (documents.getUserReceive() != null && documents.getStatus() != DocumentStatusEnum.RETAKE_ORG) {
                        documentDto.setOrgExe((documentDto.getOrgExe() == null || documentDto.getOrgExe().equals("")) ? documents.getUserReceive().getFullName() : documentDto.getOrgExe() + ", " + documents.getUserReceive().getFullName());
                    }
                }
            } else {
                Map<Long, String> map = new HashMap<>();
                orgNames.forEach(i -> {
                    if (!map.containsKey(i.getId())) {
                        map.put(i.getId(), i.getName());
                    }
                });
                content.forEach(i -> {
                    Long key = i.getDocId();
                    if (map.containsKey(key) && i.getOrgExe().equals("") && i.getDoc().getListChildren().size() <= 0) {
                        i.setOrgExe(map.get(key));
                    }
                });
            }
        }
    }

    public ListObjectDto<DocumentDto> exportExcel(Boolean expired, Boolean important, String text, String userExe, String numberOrSignL, String previewL,
                                                  Long bookIdL, Long docTypeL, String orgIssuedNameL, Long docFieldsIdL, DocumentStatusEnum docStatusIdL,
                                                  Long urgentIdL, Long securityIdL, String numberArrivalL, String orgExeL, Date startArrivalL,
                                                  Date endArrivalL, Date startIssuedL, Date endIssuedL, Date startReceived, Date endReceived,
                                                  String personSignL, HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus, Sort sort) {
        List<DocumentDto> t = exportExcelToList(expired, important, text, userExe, numberOrSignL, previewL, bookIdL,
                docTypeL, orgIssuedNameL, docFieldsIdL, docStatusIdL, urgentIdL, securityIdL, numberArrivalL, orgExeL,
                startArrivalL, endArrivalL, startIssuedL, endIssuedL, startReceived, endReceived, personSignL,
                handleType, handleStatus, sort);
        setData(t);
        return BussinessCommon.convert(t);
    }

    public List<DocumentDto> exportExcelToList(Boolean expired, Boolean important, String text, String userExe, String numberOrSignL, String previewL,
                                               Long bookIdL, Long docTypeL, String orgIssuedNameL, Long docFieldsIdL, DocumentStatusEnum docStatusIdL,
                                               Long urgentIdL, Long securityIdL, String numberArrivalL, String orgExeL, Date startArrivalL,
                                               Date endArrivalL, Date startIssuedL, Date endIssuedL, Date startReceived, Date endReceived,
                                               String personSignL, HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus, Sort sort) {
        User u = BussinessCommon.getUser();
        boolean isLibrarian = userService.isVanThuVBDen(u);
        Date now = DateTimeUtils.handleSubmit(new Date());
        Boolean delegateDoc = null;
        if (DocumentStatusEnum.DELEGATE_DOC.equals(docStatusIdL)) {
            delegateDoc = true;
            docStatusIdL = null;
        }

        Integer endTask = null;
        if (DocumentInHandleStatusEnum.HOAN_THANH.equals(handleStatus)) { // Done tab
            endTask = 2;
            handleStatus = null;
        } else if (DocumentInHandleStatusEnum.DA_XU_LY.equals(handleStatus)) { // Did tab
            endTask = 0;
        }

        List<Long> witDocument = getWithDocument(BussinessCommon.getClientId(), clericalOrg, u.getId(), isLibrarian);
        List<Long> withProcess = getWithProcess(delegateDoc, StringUtils.decodeFromUrl(orgExeL), userExe, handleType, handleStatus, endTask);
        return docRepository.findAdvanceAllNotPaging(text, expired, now, important, u.getId(), numberOrSignL,
                StringUtils.decodeFromUrl(previewL), bookIdL, docTypeL, StringUtils.decodeFromUrl(orgIssuedNameL),
                docFieldsIdL, docStatusIdL, urgentIdL, securityIdL, numberArrivalL, startArrivalL, endArrivalL,
                startIssuedL, endIssuedL, startReceived, endReceived, StringUtils.decodeFromUrl(personSignL),
                BussinessCommon.getClientId(), withProcess, witDocument, sort);
    }

    public ObjectNode exportExcelByConfig(Boolean expired, Boolean important, String text, String userExe, String numberOrSignL, String previewL,
                                          Long bookIdL, Long docTypeL, String orgIssuedNameL, Long docFieldsIdL, DocumentStatusEnum docStatusIdL,
                                          Long urgentIdL, Long securityIdL, String numberArrivalL, String orgExeL, Date startArrivalL,
                                          Date endArrivalL, Date startIssuedL, Date endIssuedL, Date startReceived, Date endReceived,
                                          String personSignL, HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus, Sort sort) {
        List<DocumentDto> t = exportExcelToList(expired, important, text, userExe, numberOrSignL, previewL, bookIdL,
                docTypeL, orgIssuedNameL, docFieldsIdL, docStatusIdL, urgentIdL, securityIdL, numberArrivalL, orgExeL,
                startArrivalL, endArrivalL, startIssuedL, endIssuedL, startReceived, endReceived, personSignL,
                handleType, handleStatus, sort);
        return rpFieldService.getData(DocumentTypeEnum.VAN_BAN_DEN, t);
    }

    @Transactional
    public void retake(Long docId, String comment, MultipartFile[] files, boolean force) {
        User u = BussinessCommon.getUser();
        Documents doc = validDocId(docId);
        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);

        if (!force && !userService.isVanThuVBDen(u)) {
            throw new RestExceptionHandler(Message.NO_PROCESS_WHEN_DONE_DOC);
        }

        if (!canRetake(doc)) {
            throw new RestExceptionHandler(Message.NO_PROCESS_WHEN_DONE_DOC);
        }

        // update node and status
        processService.updateDocByStatus(docId, DocumentStatusEnum.RETAKE_DOC);

        setStatusAndParentId(docId, DocumentStatusEnum.RETAKE_DOC);

        // comment
        saveCommentAndAttach(docId, files, comment);

        // notification
        notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);
        List<DocumentInProcess> pList = processService.getListProcessByDocId(docId);
        List<Long> userIds = pList.stream().map(DocumentInProcess::getToUser).collect(Collectors.toList());
        if (!userIds.isEmpty()) {
            notiService.addAll(userIds, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                    NotificationHandleStatusEnum.DA_THU_HOI, ModuleCodeEnum.DOC_IN_RETAKE);
        }
    }

    public void setStatusAndParentId(Long docId, DocumentStatusEnum statusEnum) {
        List<Documents> listdata = docRepository.findByParentIdAndActive(docId, BussinessCommon.getClientId());
        if (listdata != null) {
            for (Documents document : listdata
            ) {
                document.setStatus(statusEnum);
            }
            docRepository.saveAll(listdata);
        }
    }

    @Transactional
    public void restore(Long docId, String comment, MultipartFile[] files) {
        validDocId(docId);

        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);

        // update node and status
        processService.updateDocByStatus(docId, DocumentStatusEnum.DOING);

        // comment
        saveCommentAndAttach(docId, files, comment);

        // notification
        notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);
    }

    private boolean checkPermission1(Documents doc) {
        User user = BussinessCommon.getUser();
        Long userId = user.getId();
        if (DocumentStatusEnum.RETAKE_ORG.equals(doc.getStatus())) {
            return false;
        }
        if (DocumentStatusEnum.RETAKE_DOC.equals(doc.getStatus())) {
            if (roleService.existUserInModule(ModuleCodeEnum.DOC_IN_RETAKE.getName())) {
                return true;
            }
        }
        if (userId.equals(doc.getPersonEnterId())
                || (clericalOrg ? clericalOrgService.isClericalOrg(user.getId(), doc.getOrgReceiveId())
                : rService.isVanThuVBDenByOrg(user, doc.getOrgReceiveId()))) {
            return true;
        }
        DocumentInProcess p = processService.findByRelatedAndDocId(userId, doc.getId(), null);
        if (p != null) {
            return true;
        }
        //check văn bản chờ cho ý kiến
        if (manipulationService.hasRelatedIdea(doc.getId(), userId)) {
            return true;
        }

        return roleService.checkCanRetake(doc.getOrgReceiveId(), DocumentTypeEnum.VAN_BAN_DEN);
    }

    public boolean checkPermission(Long docId) {
        Documents doc = validDocId(docId);
        return checkPermission(doc);
    }

    public boolean checkPermission(Documents doc) {
        Set<Long> docInChecked = new HashSet<>();
        Set<Long> docOutChecked = new HashSet<>();
        Set<Long> taskChecked = new HashSet<>();
        Set<Long> folderChecked = new HashSet<>();
        return checkPermission(doc, docInChecked, docOutChecked, taskChecked, folderChecked);
    }

    public boolean checkPermission(Long docId, Set<Long> docInChecked, Set<Long> docOutChecked, Set<Long> taskChecked, Set<Long> folderChecked) {
        Optional<Documents> doc = docRepository.findById(docId);
        if (doc.isPresent()) {
            return checkPermission(doc.get(), docInChecked, docOutChecked, taskChecked, folderChecked);
        }
        return false;
    }

    public boolean checkPermission(Documents doc, Set<Long> docInChecked, Set<Long> docOutChecked, Set<Long> taskChecked, Set<Long> folderChecked) {

        // Check calendar join
        if (calendarService.isMemberRelatedDoc(getUserOrListVanThuBan(), doc.getId(), DocumentTypeEnum.VAN_BAN_DEN)) {
            return true;
        }

        if (!docInChecked.contains(doc.getId())) {
            docInChecked.add(doc.getId());
            if (checkPermission1(doc)) {
                return true;
            }
        }
        // Check văn bản đi phúc đáp
        for (DocumentInOut docOut : doc.getListResponseDoc()) {
            if (!docOutChecked.contains(docOut.getDocOut().getId()) && docOutService.checkPermission(docOut.getDocOut(), docInChecked, docOutChecked, taskChecked, folderChecked)) {
                return true;
            }
        }
        // Check văn bản cha
        if (doc.getParentId() != null) {
            return checkPermission(doc.getParent(), docInChecked, docOutChecked, taskChecked, folderChecked);
        }
        // Check công việc giải quyết văn bản đến => Công việc trả lời.
        List<Task> listRelateTask = taskService.getListTaskByDocIdAndDocType(doc.getId(), true);
        for (Task t : listRelateTask) {
            if (!taskChecked.contains(t.getId()) && taskService.checkPermission(t, docInChecked, docOutChecked, taskChecked, folderChecked)) {
                return true;
            }
        }
        // Check hồ sơ chứa văn bản
        return hsFolderService.checkPermissionByDocId(doc.getId(), DocumentTypeEnum.VAN_BAN_DEN, docInChecked, docOutChecked, taskChecked, folderChecked, false);
    }

    public void updateDeadline(Long docId, Date deadline, DeadlineHandleTypeEnum type) {
        User user = BussinessCommon.getUser();

        Documents doc = validDocId(docId);

        if (deadline == null) {
            throw new RestExceptionHandler("Ngày gia hạn bắt buộc nhập");
        }
        if (DateTimeUtils.handleSubmit(deadline).compareTo(DateTimeUtils.handleSubmit(new Date())) < 0) {
            throw new RestExceptionHandler("Ngày gia hạn phải lớn hơn hoặc bằng ngày hiện tại");
        }
        if (DateTimeUtils.handleSubmit(deadline).compareTo(DateTimeUtils.handleSubmit(doc.getCreateDate())) < 0) {
            throw new RestExceptionHandler("Ngày gia hạn phải lớn hơn hoặc bằng ngày văn bản");
        }
        DocumentInProcess oProcess;
        if (setDeadline) { // for BCY

            switch (type) {
                case SUPPERVISOR:
                    if (rService.isSupervisor(user)) {
                        doc.setDeadline(deadline);
                        docRepository.save(doc);
                    }
                    break;
                case DELEGATE:
                    oProcess = processService.findByDelegateAndDocId(user.getId(), docId, null);
                    if (oProcess == null) {
                        throw new RestExceptionHandler(Message.NO_PERMISSION);
                    }

                    updateDeadline(deadline, oProcess);
                    break;
                case THREADS:
                    oProcess = processService.findByToUserAndDocId(user.getId(), docId);
                    if (oProcess == null) {
                        throw new RestExceptionHandler(Message.NO_PERMISSION);
                    }

                    if (!processService.canUpdateDeadline(oProcess)) {
                        throw new RestExceptionHandler(Message.NO_PERMISSION);
                    }

                    updateDeadline(deadline, oProcess);
                    break;
                default:
                    throw new RestExceptionHandler(Message.NO_PERMISSION);
            }

        } else {
            oProcess = processService.findByDelegateAndDocId(user.getId(), docId, null);
            if (!rService.isSupervisor(user) && !processService.canUpdateDeadline(oProcess)) {
                throw new RestExceptionHandler(Message.NO_PERMISSION);
            }
            doc.setDeadline(deadline);
            docRepository.save(doc);
        }

        // add tracking
        trackingServices.save(docId, DocumentInTrackingEnum.EXTENSION, user.getId());
    }

    private void updateDeadline(Date deadline, DocumentInProcess oProcess) {
        oProcess.setDeadline(deadline);
        processService.save(oProcess);
    }

    public Page<FindDocDto> findAll(FindDocDto dto, Integer page) {
        Sort sort = Sort.by(Direction.DESC, "updateDate", "createDate");
        User u = BussinessCommon.getUser();
        dto = dto.convert(dto);
        dto.setCreateFrom(DateTimeUtils.getStartDate(dto.getCreateFrom()));
        dto.setDateIssuedFrom(DateTimeUtils.getStartDate(dto.getDateIssuedFrom()));
        dto.setDateArrivalFrom(DateTimeUtils.getStartDate(dto.getDateArrivalFrom()));
        dto.setCreateTo(DateTimeUtils.getEndDate(dto.getCreateTo()));
        dto.setDateIssuedTo(DateTimeUtils.getEndDate(dto.getDateIssuedTo()));
        dto.setDateArrivalTo(DateTimeUtils.getEndDate(dto.getDateArrivalTo()));
        Page<Documents> docList = docRepository.findAll(dto.getNumberOrSign(), dto.getPreview(), dto.getDocFieldsId(),
                dto.getDocStatusId(), dto.getUrgentId(), dto.getSecurityId(), u.getId(), dto.getPlaceSendId(),
                dto.getDateArrivalFrom(), dto.getDateArrivalTo(), dto.getDateIssuedFrom(), dto.getDateArrivalTo(),
                dto.getDateReceivedFrom(), dto.getDateReceivedTo(), u.getClientId(),
                BussinessCommon.castToPageable(page, sort));
        if (BussinessCommon.isEmptyPage(docList)) {
            return new PageImpl<>(Collections.emptyList(), BussinessCommon.castToPageable(page),
                    docList.getTotalElements());
        }
        int no = 0;
        List<FindDocDto> rsList = new ArrayList<>();
        for (Documents documents : docList) {
            FindDocDto rs = new FindDocDto(documents);
            rs.setNo(++no);
            rsList.add(rs);
        }
        return new PageImpl<>(rsList, BussinessCommon.castToPageable(page), docList.getTotalElements());
    }

    public Page<DocInRetakeDto> listRetake(DocumentStatusEnum[] docStatus, Pageable pageable) {
        User u = BussinessCommon.getUser();
        return docRepository.listRetake(false, docStatus, u.getId(), u.getOrg(), u.getClientId(), pageable);
    }

    public Page<DocInRetakeDto> quickRetake(DocumentStatusEnum[] docStatus, String text, Pageable pageable) {
        User u = BussinessCommon.getUser();
        return docRepository.quickRetake(false, text, docStatus, u.getId(), u.getOrg(), u.getClientId(), pageable);
    }

    public Page<DocInRetakeDto> searchRetake(DocumentStatusEnum[] docStatus, Pageable pageable, String numberOrSign,
                                             String preview, Long bookId, Long docTypeId, String orgIssuedName, Long docFieldsId, Long urgentId,
                                             Long securityId, String numberArrival, Date startArrival, Date endArrival, Date startIssued, Date endIssued,
                                             String personSign) {
        User u = BussinessCommon.getUser();
        return docRepository.searchRetake(false, numberOrSign, preview, bookId, docTypeId, orgIssuedName, docFieldsId, null,
                urgentId, securityId, numberArrival, startArrival, endArrival, startIssued, endIssued, personSign,
                docStatus, u.getId(), u.getOrg(), u.getClientId(), pageable);
    }

    private List<Long> getUserIdListByArr(String[] arr) {
        List<Long> ids = new ArrayList<>();
        if (!BussinessCommon.isEmptyArr(arr)) {
            for (String m : arr) {
                Long assign = BussinessCommon.convert(m, 1);
                Long delegate = BussinessCommon.convert(m, 2);
                if (assign != null) {
                    ids.add(assign);
                }

                if (delegate != null) {
                    ids.add(delegate);
                }
            }
        }

        return ids;
    }

    private List<User> getUserByArr(String[] main, String[] support, String[] show, Long[] direction) {
        List<Long> tmp = new ArrayList<>();
        List<Long> mainIds = getUserIdListByArr(main);
        List<Long> supportIds = getUserIdListByArr(support);
        List<Long> showIds = getUserIdListByArr(show);
        tmp.addAll(mainIds);
        tmp.addAll(supportIds);
        tmp.addAll(showIds);
        if (!BussinessCommon.isEmptyArr(direction)) {
            tmp.addAll(Arrays.asList(direction));
        }
        return userService.findByIds(tmp, true);
    }

    public List<Documents> transferHandleList(Long[] docIdList, String comment, String cmtContent, String[] main, String[] support,
                                              String[] show, Long node, MultipartFile[] files, Long[] orgMain, Long[] orgSupport, Long[] orgShow,
                                              Date deadline, Long[] direction, Boolean requestReview) {
        if (BussinessCommon.isEmptyArr(docIdList)) {
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);
        }

        User u = BussinessCommon.getUser();

        List<Documents> docList = processService.validDocIdList(Arrays.asList(docIdList));
        HandleTypeEnum handleTypeEnum = getDocumentHandleType(docList.get(0));
        this.all = new HashSet<>();
        if (this.mainRequire && ArrayUtils.isEmpty(main) && ArrayUtils.isEmpty(orgMain) && ArrayUtils.isEmpty(direction))
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);

        boolean hasMain = !ArrayUtils.isEmpty(main) || !ArrayUtils.isEmpty(orgMain);
        List<User> userList = getUserByArr(main, support, show, direction);
        List<HandlerDto> uMain = hasMain ? add(orgMain, main, userList, true) : getHandleDto(direction, userList);
        List<HandlerDto> uSupports = add(orgSupport, support, userList, false);
        List<HandlerDto> uShows = add(orgShow, show, userList, false);
        List<HandlerDto> uDirections = !BussinessCommon.isEmptyArr(direction) && hasMain ? getHandleDto(direction, userList) : new ArrayList<>();

        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);
//        BussinessCommon.validLengthData(cmtContent, "Ý kiến xử lý", 2000);
        for (Documents doc : docList) {
            if (!u.getId().equals(doc.getPersonEnterId())) {
                long org1 = doc.getPersonEnter().getOrg();
                if (!userService.isVanThuVBDenByOrg(u, org1)) {
                    throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
                }
            }
        }

        // update old process
        List<DocumentInProcess> pfirstList = processService.findByDocIdAndLastedStep(Arrays.asList(docIdList), HandleTypeEnum.MAIN);
        if (pfirstList.isEmpty()) {
            throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
        }

        processService.update(pfirstList, DocumentInHandleStatusEnum.DA_XU_LY, node);
        trackingServices.save(docList, DocumentInTrackingEnum.TRANSFER, u);

        // add new process & tracking
        Long preNode = null;
        for (Documents doc : docList) {
            preNode = doc.getPreNode();
            for (DocumentInProcess p : pfirstList) {
                if (p.getDocId().equals(doc.getId())) {
                    addProcesss(u, preNode, node, doc, p.getStep() + 1, uMain, uSupports, uShows, uDirections, comment, cmtContent, DocumentCommentTypeEnum.CHUYEN_XU_LY, files, deadline, Constant.START_STEP + 1, !hasMain, requestReview, handleTypeEnum);
                }
            }
            // update node & status
            doc.setNode(node);
            doc.setPreNode(preNode);
            doc.setStatus(DocumentStatusEnum.DOING);
        }

        this.handleByConfidentialDoc2Receiver(docList, node, userList);

        this.all.clear();
        return docRepository.saveAll(docList);
    }

    private void addProcesss(User user, Long preNode, Long node, Documents doc, Integer stepsFirst, List<HandlerDto> main,
                             List<HandlerDto> supports, List<HandlerDto> shows, List<HandlerDto> directions, String comment, String cmtContent, DocumentCommentTypeEnum type, MultipartFile[] files, Date deadline, Integer transferStep, boolean hasMain, Boolean requestReview, HandleTypeEnum handleType) {
        Long docId = doc.getId();

        // add process for new
        processService.saveList(user, preNode, node, main, handleType, docId, stepsFirst, deadline, transferStep, hasMain, requestReview);
        processService.saveList(user, preNode, node, supports, HandleTypeEnum.SUPPORT, docId, stepsFirst, deadline, null, false, false);
        processService.saveList(user, preNode, node, shows, HandleTypeEnum.SHOW, docId, stepsFirst, deadline, null, false, false);
        processService.saveList(user, preNode, node, directions, HandleTypeEnum.DIRECTION, docId, stepsFirst, deadline, null, false, false);

        // add tracking for new
        List<User> mainList = main.stream().map(HandlerDto::getAssignHandler).collect(Collectors.toList());
        List<User> supportsList = supports.stream().map(HandlerDto::getAssignHandler).collect(Collectors.toList());
        List<User> showsList = shows.stream().map(HandlerDto::getAssignHandler).collect(Collectors.toList());
        List<User> directionList = directions.stream().map(HandlerDto::getAssignHandler).collect(Collectors.toList());
        trackingServices.save(doc, DocumentInTrackingEnum.INCOMING, mainList, null, Constant.OTHER_ACTION_TYPE);
        trackingServices.save(doc, DocumentInTrackingEnum.INCOMING, supportsList, null, Constant.OTHER_ACTION_TYPE);
        trackingServices.save(doc, DocumentInTrackingEnum.INCOMING, showsList, null, Constant.OTHER_ACTION_TYPE);
        trackingServices.save(doc, DocumentInTrackingEnum.DIRECTION, directionList, null, Constant.OTHER_ACTION_TYPE);

        // comment
        if (!StringUtils.isNullOrEmpty(comment) || !StringUtils.isNullOrEmpty(cmtContent)) {
            saveTransferCommentAndAttach(docId, files, comment, cmtContent, type);
        }

        // get list lead when choose normal acc
        List<Long> assignNotLeadListAll = new ArrayList<>();

        DocumentTypeEnum documentTypeEnum = null;
        ModuleCodeEnum moduleCodeEnum = null;

        switch (handleType) {
            case MAIN:
                documentTypeEnum = DocumentTypeEnum.VAN_BAN_DEN;
                moduleCodeEnum = ModuleCodeEnum.DOC_OUT_MAIN;
                break;
            case INTERNAL_INCOMING:
                documentTypeEnum = DocumentTypeEnum.VAN_BAN_NOI_BO;
                moduleCodeEnum = ModuleCodeEnum.DOC_INTERNAL_INCOMING;
                break;
            case INTERNAL_ISSUED_INCOMING:
                documentTypeEnum = DocumentTypeEnum.VAN_BAN_DEN_NOI_BO;
                moduleCodeEnum = ModuleCodeEnum.DOC_INTERNAL_ISSUED_INCOMING;
                break;
        }

        // notification
//		notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);
        if (!BussinessCommon.isEmptyList(main)) {
            List<Long> delegate = getHandlerByType(main, 2);
            List<Long> assign = getHandlerByType(main, 1);
            List<Long> assignNotLead = getHandlerByType(main, 3);
            if (!BussinessCommon.isEmptyList(assign)) {
                notiService.addAll(assign, docId, doc.getPreview(), documentTypeEnum,
                        NotificationHandleStatusEnum.XU_LY_CHINH, getByType(doc.getMergedLines(), moduleCodeEnum));
            }

            if (!BussinessCommon.isEmptyList(delegate)) {
                notiService.addAll(delegate, docId, doc.getPreview(), documentTypeEnum,
                        NotificationHandleStatusEnum.XU_LY_CHINH_UQ, getByType(doc.getMergedLines(), moduleCodeEnum));
            }

            if (this.autoAddLeadTransfer && !BussinessCommon.isEmptyList(assignNotLead)) {
                assignNotLeadListAll.addAll(assignNotLead);
            }
        }

        if (!BussinessCommon.isEmptyList(supports)) {
            List<Long> delegate = getHandlerByType(supports, 2);
            List<Long> assign = getHandlerByType(supports, 1);
            List<Long> assignNotLead = getHandlerByType(supports, 3);
            if (!BussinessCommon.isEmptyList(assign)) {
                notiService.addAll(assign, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                        NotificationHandleStatusEnum.PHOI_HOP, getByType(doc.getMergedLines(), ModuleCodeEnum.DOC_OUT_COMBINE));
            }

            if (!BussinessCommon.isEmptyList(delegate)) {
                notiService.addAll(delegate, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                        NotificationHandleStatusEnum.PHOI_HOP_UQ, getByType(doc.getMergedLines(), ModuleCodeEnum.DOC_OUT_COMBINE));
            }

            if (this.autoAddLeadTransfer && !BussinessCommon.isEmptyList(assignNotLead)) {
                assignNotLeadListAll.addAll(assignNotLead);
            }
        }

        if (!BussinessCommon.isEmptyList(shows)) {
            List<Long> assign = getHandlerByType(shows, 1);
            List<Long> assignNotLead = getHandlerByType(shows, 3);
            if (!BussinessCommon.isEmptyList(assign)) {
                notiService.addAll(assign, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                        NotificationHandleStatusEnum.NHAN_DE_BIET, getByType(doc.getMergedLines(), ModuleCodeEnum.DOC_OUT_KNOW));
            }

            if (this.autoAddLeadTransfer && !BussinessCommon.isEmptyList(assignNotLead)) {
                assignNotLeadListAll.addAll(assignNotLead);
            }
        }

        if (!BussinessCommon.isEmptyList(directions)) {
            List<Long> assign = getHandlerByType(directions, 1);
            if (!BussinessCommon.isEmptyList(assign)) {
                notiService.addAll(assign, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                        NotificationHandleStatusEnum.CHI_DAO, getByType(doc.getMergedLines(), ModuleCodeEnum.DOC_OUT_DIRECTION));
            }
        }

        // get list lead when choose normal acc
        if (this.autoAddLeadTransfer && !BussinessCommon.isEmptyList(assignNotLeadListAll)) {
            List<Long> tmp = new ArrayList<>();
            tmp.addAll(this.all);
            List<User> uList = userService.getLeadByListUserId(assignNotLeadListAll, tmp);

            if (!uList.isEmpty()) {
                List<Long> uMainList = uList.stream().map(User::getId).collect(Collectors.toList());
                List<Long> userIdRelatedDoc = processService.findByRelatedAndDocId(uMainList, docId);
                if (!userIdRelatedDoc.isEmpty()) {
                    uMainList.removeAll(userIdRelatedDoc);
                }

                if (!uMainList.isEmpty()) {
                    processService.saveList1(node, uMainList, HandleTypeEnum.SHOW, docId, stepsFirst);
                    trackingServices.save(doc, DocumentInTrackingEnum.INCOMING, uList, null, Constant.OTHER_ACTION_TYPE);
                    notiService.addAll(uMainList, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                            NotificationHandleStatusEnum.NHAN_DE_BIET, getByType(doc.getMergedLines(), ModuleCodeEnum.DOC_OUT_KNOW));
                }
            }
        }
    }

    private ModuleCodeEnum getByType(Boolean mergedLines, ModuleCodeEnum old) {
        if (Boolean.TRUE.equals(mergedLines)) {
            return ModuleCodeEnum.DOCUMENT_OUT_INTERNAL;
        }
        return old;
    }

    // 1 - người ủy quyền, 2- người được ủy quyền, 3- người ủy quyền (không phải
    // lead)
    private List<Long> getHandlerByType(List<HandlerDto> list, int type) {
        List<Long> delegateList = new ArrayList<>();
        List<Long> assignList = new ArrayList<>();
        List<Long> assignNotLeadList = new ArrayList<>();
        for (HandlerDto dto : list) {
            if (dto.getAssignHandler() != null && type == 1) {
                assignList.add(dto.getAssignHandler().getId());
            }
            if (dto.getDelegateHandler() != null && type == 2) {
                delegateList.add(dto.getDelegateHandler().getId());
            }
            if (dto.getAssignHandler() != null && type == 3 && !dto.isLead()) {
                assignNotLeadList.add(dto.getAssignHandler().getId());
            }
        }
        if (type == 1) {
            return assignList;
        }
        if (type == 2) {
            return delegateList;
        }
        if (type == 3) {
            return assignNotLeadList;
        }
        return null;
    }

    public ListObjectDto<DocumentDto> getListDelegatedDocs(Integer dayLeft, Boolean important, Integer typeHandle, Integer statusHandle,
                                                           String text, String numberOrSign, String preview, Long docType, Long docFields, Pageable page) {
        Page<DocumentDto> rsPage = null;
        List<DocumentStatusEnum> docStatus = null;
        List<DocumentInHandleStatusEnum> handleStatusList = null;
        List<HandleTypeEnum> handleType = null;
        List<Long> delegateList = new ArrayList<>();
        delegateList.add(BussinessCommon.getUserId());
        if (typeHandle.intValue() == 0) { // main type
            handleType = HandleTypeEnum.getListHandleType(HandleTypeEnum.MAIN);

            if (statusHandle.intValue() == 1) { // doing - not yet
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("CHO_XU_LY", "DANG_XU_LY",
                        "CHUYEN_DON_VI", "CHO_DANH_GIA", "XIN_DANH_GIA", "DG_CHAP_NHAN", "DG_TU_CHOI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
            }
        } else if (typeHandle.intValue() == 1) { // support type
            handleType = HandleTypeEnum.getListHandleType(HandleTypeEnum.SUPPORT);

            if (statusHandle.intValue() == 1) { // doing - not yet
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("CHO_XU_LY", "DANG_XU_LY");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
            }
        } else if (typeHandle.intValue() == 3) { // support + main type
            handleType = HandleTypeEnum.getListHandleType(HandleTypeEnum.MAIN, HandleTypeEnum.SUPPORT);

            if (statusHandle.intValue() == 0) { // done
                handleStatusList = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY", "DA_TRA_LAI");
                docStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.DONE,
                        DocumentStatusEnum.RETURN_DOC);
            }
        }

        if (!StringUtils.isNullOrEmpty(text)) {
            rsPage = docRepository.findProcessBasic1(dayLeft, DocumentTypeEnum.VAN_BAN_DEN, BussinessCommon.getUserId(),
                    docStatus, delegateList, handleStatusList, handleType, text, BussinessCommon.getClientId(), page);
        } else {
            rsPage = docRepository.findProcessAdvance1(dayLeft, important, DocumentTypeEnum.VAN_BAN_DEN,
                    BussinessCommon.getUserId(), docStatus, delegateList, handleStatusList, handleType, numberOrSign,
                    preview, docType, docFields, BussinessCommon.getClientId(), page);
        }

        if (!BussinessCommon.isEmptyPage(rsPage)) {
            List<DocumentDto> rsList = rsPage.getContent();
            rsList.forEach(dto -> {
                Documents doc = dto.getDoc();
                if (doc == null) {
                    return;
                }
//				dto.setCanFinish(checkCanFinish(doc));
            });
        }
        return BussinessCommon.paging(rsPage);
    }

    private List<HandlerDto> add(Long[] orgs, String[] users, List<User> userList, boolean isRequire) {
        List<HandlerDto> dtos = new ArrayList<>();
        dtos.addAll(getHandler(users, userList, isRequire));
        dtos.addAll(getListLead(orgs));
        return dtos;
    }

    private Long isExist(Long toCheck) {
        boolean isExist = all.contains(toCheck);
        if (isExist) {
            return null;
        }
        if (toCheck != null) {
            all.add(toCheck);
        }
        return toCheck;
    }

    // 1: assign - 2 Delegate
    private List<HandlerDto> getHandler(String[] arr, List<User> userList, boolean isRequire) {
        List<HandlerDto> rs = new ArrayList<>();
        if (BussinessCommon.isEmptyArr(arr) || BussinessCommon.isEmptyList(userList)) {
            return rs;
        }
        for (String str : arr) {
            Long assign = BussinessCommon.convert(str, 1);
            Long delegate = BussinessCommon.convert(str, 2);
            if (isRequire && assign == null) {
                throw new RestExceptionHandler(Message.NO_INPUT_DATA);
            }

            assign = isExist(assign);
            delegate = isExist(delegate);

            if (assign != null) {
                User assignUser = userService.validUs(BussinessCommon.convert(str, 1), userList);
                User delegateUser = null;
                if (delegate != null) {
                    delegateUser = userService.validUs(BussinessCommon.convert(str, 2), userList);
                }
                HandlerDto dto = new HandlerDto(assignUser, delegateUser, assignUser.isLead());
                rs.add(dto);
            }
        }
        return rs;
    }

    private List<HandlerDto> getListLead(Long[] orgArr) {
        List<HandlerDto> rs = new ArrayList<>();
        if (ArrayUtils.isEmpty(orgArr)) return rs;
        List<User> userList = userService.getListLeadByOrg(orgArr);
        if (BussinessCommon.isEmptyList(userList)) {
            throw new RestExceptionHandler(Message.SET_DEFAULT_NOT_YET);
        }

        HashMap<Long, User> map = new HashMap<>();
        userList.forEach(i -> map.put(i.getOrg(), i));

        for (Long org : orgArr) {
            if (map.get(org) == null) {
                throw new RestExceptionHandler(Message.SET_DEFAULT_NOT_YET);
            }
        }

        userList.forEach(j -> {
            if (isExist(j.getId()) != null) {
                HandlerDto dto = new HandlerDto(j, null, j.isLead());
                rs.add(dto);
            }
        });

        return rs;
    }

    private List<HandlerDto> getHandleDto(Long[] arr, List<User> userList) {
        List<HandlerDto> rs = new ArrayList<>();
        if (arr == null)
            return rs;
        for (Long a : arr) {
            if (isExist(a) != null) {
                rs.add(new HandlerDto(userService.validUs(a, userList)));
            }
        }

        return rs;
    }

    public Page<FindDocDto> findAllDoc(FindDocDto dto, Pageable page) {
        User u = BussinessCommon.getUser();
        dto = dto.convert(dto);
        Date now = DateTimeUtils.handleSubmit(new Date());
        List<Long> orgIds = orgService.orgAndSub(u.getOrg());
        List<Long> listUser = clericalOrgRepo.getClericalOrgByOrgIdAndClientId(orgService.getRootOrgId(u.getOrg()), BussinessCommon.getClientId());
        for (Long userIdVTDV : listUser) {
            User userOrg = userService.findByUserId(userIdVTDV);
            if (userOrg != null) {
                orgIds.add(userOrg.getOrg());
            }
        }
        List<Documents> documentsMap = docRepository.findAllIdDoc(dto.getExpired(), now, dto.getNumberOrSign(), dto.getBookId(), dto.getPreview(),
                dto.getDocFieldsId(), dto.getDocStatusId(), dto.getUrgentId(), dto.getSecurityId(), u.getClientId(), dto.getNumberArrival(), dto.getCreateFrom(), dto.getCreateTo(),
                dto.getDateIssuedFrom(), dto.getDateIssuedTo(), dto.getDateReceivedFrom(), dto.getDateReceivedTo(), dto.getPlaceSend(), orgIds, Constant.VAN_THU_MAIN, orgService.getRootOrgId(u.getOrg()), dto.getOrgReceiveId());
        int no = 0;
        List<FindDocDto> rsList = new ArrayList<>();
        for (Documents documents : documentsMap) {
            FindDocDto rs = new FindDocDto(documents);
            rs.setNo(++no);
            rsList.add(rs);
        }
        HashMap<Long, Long> hashMap = new HashMap<>();
        List<Long> listLong = new ArrayList<>();
        for (FindDocDto findDocDto : rsList) {
            if (findDocDto.getParentId() != null) {
                if (!hashMap.containsKey(findDocDto.getParentId())) {
                    hashMap.put(findDocDto.getParentId(), findDocDto.getParentId());
                    listLong.add(findDocDto.getId());
                }
            } else {
                listLong.add(findDocDto.getId());
            }
        }
        List<Long> orgIdBan = orgService.orgAndSub(orgService.getRootOrgId(BussinessCommon.getOrgId()));
        List<Long> userBanThuBan = userRepository.getUsersByNameCategory(Constant.VAN_THU_MAIN, orgIdBan);
        Page<Documents> docList = docRepository.findAllDoc(listLong, userBanThuBan, page);
        int no1 = 0;
        List<FindDocDto> objectArrayList = new ArrayList<>();
        hashMap = new HashMap<>();
        for (Documents documents : docList) {
            FindDocDto rs = new FindDocDto(documents);
            rs.setNo(++no1);
            objectArrayList.add(rs);
        }

        return new PageImpl<>(objectArrayList, page, docList.getTotalElements());
    }

    public Long getNodeProces(DocumentInProcess p) {
        if (p == null) return null;
        return p.getNode();
    }

    @Transactional
    //special = true => Sau khi đã chuyển đơn vị nhưng muốn chuyển thêm đơn vị khác.
    public boolean orgTransfer(List<Long> listIds, String comment, List<Long> listOrg, Long node, MultipartFile[] files,
                               boolean isDelegate, boolean special) {
        User user = BussinessCommon.getUser();

        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);

        if (BussinessCommon.isEmptyList(listOrg)) {
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);
        }

        for (Long docId : listIds) {
            Documents doc = validDocId(docId);
            DocumentInProcess oProcess;
            boolean isAddForDelegate = false;
            List<Long> listOrgNew = new ArrayList<>();
            if (!special) {
                if (isDelegate) {
                    oProcess = processService.findByDelegateAndDocId(user.getId(), docId, HandleTypeEnum.MAIN);
                    if (oProcess == null || !processService.isType(oProcess.getHandleType(), HandleTypeEnum.MAIN)
                            || !processService.isStatus(oProcess.getHandleStatus(), processService.HANDLE_STATUS_NOT_YET)) {
                        throw new RestExceptionHandler(Message.NO_DELEGATE);
                    }
                    isAddForDelegate = true;
                } else {
                    oProcess = processService.findByToUserAndDocId(user.getId(), docId);
                    if (oProcess == null || !processService.isType(oProcess.getHandleType(), HandleTypeEnum.MAIN)
                            || !processService.isStatus(oProcess.getHandleStatus(), processService.HANDLE_STATUS_NOT_YET)) {
                        throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
                    }
                }

                List<DocumentInProcess> prList = processService.update(docId, oProcess.getStep(),
                        DocumentInHandleStatusEnum.CHUYEN_DON_VI, DocumentInHandleStatusEnum.DA_XU_LY);

                notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);
                List<Long> userId = prList.stream().map(DocumentInProcess::getToUser).collect(Collectors.toList());
                List<User> userIds = userService.findByIds(userId, true);
                trackingServices.save(doc, DocumentInTrackingEnum.FINISH, userIds, user.getId(),
                        Constant.TRANSFER_HANDLE_TYPE);

                // add or update record for delegate
                if (isAddForDelegate) {
                    processService.saveFrDelegate(oProcess, HandleTypeEnum.MAIN,
                            DocumentInHandleStatusEnum.CHUYEN_DON_VI_UQ);
                    trackingServices.save(docId, DocumentInTrackingEnum.TRANSFER_UQ, user.getId());
                }
            } else {
                // Check process
                if (isDelegate) {
                    oProcess = processService.findByDelegateAndDocId(user.getId(), docId, HandleTypeEnum.MAIN);
                    if (oProcess == null || !processService.isType(oProcess.getHandleType(), HandleTypeEnum.MAIN)
                            || !DocumentInHandleStatusEnum.CHUYEN_DON_VI.equals(oProcess.getHandleStatus())) {
                        throw new RestExceptionHandler(Message.NO_DELEGATE);
                    }
                } else {
                    oProcess = processService.findByToUserAndDocId(user.getId(), docId);
                    if (oProcess == null || !processService.isType(oProcess.getHandleType(), HandleTypeEnum.MAIN)
                            || !DocumentInHandleStatusEnum.CHUYEN_DON_VI.equals(oProcess.getHandleStatus())
                    ) {
                        throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
                    }
                }

                // Lọc listOrgIds
                List<Long> orgs = new ArrayList<>();
                for (Documents child : doc.getListChildren()) {
                    if (!DocumentStatusEnum.RETAKE_ORG.equals(child.getStatus())) {
                        orgs.add(child.getOrgReceiveId());
                    }
                }
                for (int i = 0; i < listOrg.size(); i++) {
                    if (!orgs.contains(listOrg.get(i))) {
                        listOrgNew.add(listOrg.get(i));
                    }
                }
                listOrg.clear();
                listOrg.addAll(listOrgNew);
            }
            saveCommentAndAttach(docId, files, comment);

            List<Documents> listDocs = docRepository.saveAll(saveDocsByOrgIds(doc, node, listOrg));

//			attService.addListAttachment(doc.getAttachments(), listDocs);
            for (Documents d : listDocs) {
                // Add notification cho văn thư
                // Lấy danh sách văn thư theo org
                List<Long> listUserIds = clericalOrg ? clericalOrgService.getClericalOrgByOrgId(d.getOrgReceiveId())
                        : userService.getListIdsVanThuVBDenByOrg(d.getOrgReceiveId());
                if (listUserIds != null) {
                    List<Notification> listNotifications = new ArrayList<>();
                    for (long id : listUserIds) {
                        Notification noti = new Notification();
                        noti.setUserId(id);
                        noti.setDocId(d.getId());
                        noti.setDescription(d.getPreview());
                        noti.setDocType(DocumentTypeEnum.VAN_BAN_DEN);
                        noti.setDocStatus(NotificationHandleStatusEnum.CHO_TIEP_NHAN);
                        noti.setModuleCode(ModuleCodeEnum.DOC_OUT_LIST);
                        listNotifications.add(noti);
                    }
                    notiService.saveAll(listNotifications);
                }
            }

            trackingServices.save(listDocs, DocumentInTrackingEnum.TRANSFER, user);
        }

        return true;
    }

    private List<Documents> saveDocsByOrgIds(Documents doc, Long node, List<Long> orgIds) {
        List<Documents> listDocs = new ArrayList<>();
        orgIds.forEach(i -> {
            Documents nDoc = new Documents(doc, node, doc.getId(), DocumentStatusEnum.WAIT_RECEIVE, i);
            listDocs.add(nDoc);
        });
        return listDocs;
    }

    private List<Documents> saveDocsByOrgIdsorgTransfer2(Documents doc, Long node, List<List<Long>> orgIds) {
        List<Documents> listDocs = new ArrayList<>();
        if (orgIds.size() == 3 && orgIds.get(0).size() == 1) {
            Documents nDoc = new Documents();
            if (orgIds.get(2).get(0) == 0) {
                nDoc = new Documents(doc, node, doc.getId(), DocumentStatusEnum.WAIT_RECEIVE, orgIds.get(0).get(0), orgIds.get(1).get(0), null);
            } else {
                String[] arrays = new String[]{orgIds.get(0).get(0).toString()};
                nDoc = new Documents(doc, node, doc.getId(), DocumentStatusEnum.WAIT_RECEIVE, null, orgIds.get(1).get(0), orgIds.get(0).get(0));
                transfer2(doc.getId(), null, null, arrays, null, null, node, null, null, null, null, null, null, false);
            }
            listDocs.add(nDoc);
        } else {
            orgIds.forEach(i -> {
                Documents nDoc = new Documents();
                if (i.get(2) == 0) {
                    nDoc = new Documents(doc, node, doc.getId(), DocumentStatusEnum.WAIT_RECEIVE, i.get(0), i.get(1), null);

                } else {
                    String[] arrays = new String[]{i.get(0).toString()};
                    nDoc = new Documents(doc, node, doc.getId(), DocumentStatusEnum.WAIT_RECEIVE, null, i.get(1), i.get(0));
                    transfer2(doc.getId(), null, null, arrays, null, null, node, null, null, null, null, null, null, false);
                }
                listDocs.add(nDoc);
            });
        }

        return listDocs;
    }

    private void saveTransferCommentAndAttach(Long docId, MultipartFile[] files, String comment, String cmtContent, DocumentCommentTypeEnum type) {
        if (cmtContent != null) {
            DocumentComment cmt = new DocumentComment();
            cmt.setDocId(docId);
            cmt.setComment(comment);
            cmt.setCmtContent(cmtContent);
            cmt.setType(type);
            cmt.setIsTransfer(true);
            cmt = cmtService.saveCmt(cmt);
            if (files != null && files.length > 0) {
                attachCmtService.addListAttachmentComment(files, cmt.getId());
            }
        }
    }

    private void saveCommentAndAttach(Long docId, MultipartFile[] files, String comment) {
        if (comment != null && !comment.equals("") || files != null && files.length > 0) {
            DocumentComment cmt = new DocumentComment();
            cmt.setDocId(docId);
            cmt.setComment(comment);
            cmt = cmtService.saveCmt(cmt);
            attachCmtService.addListAttachmentComment(files, cmt.getId());
        }
    }

    private void saveCommentAndAttach(Long docId, MultipartFile[] files, String comment, DocumentCommentTypeEnum type) {
        if (comment != null && !comment.equals("") || files != null && files.length > 0) {
            DocumentComment cmt = new DocumentComment();
            cmt.setDocId(docId);
            cmt.setComment(comment);
            cmt.setType(type);
            cmt = cmtService.saveCmt(cmt);
            attachCmtService.addListAttachmentComment(files, cmt.getId());
        }
    }

    private void saveCommentChuyenDonVi(Long docId, MultipartFile[] files, String comment, DocumentCommentTypeEnum type, List<List<Long>> orLists) {
        String c = "";
        for (List<Long> orgListIds : orLists) {
            if ((orLists.get(0).size() > 1 && orgListIds.get(2) == 0) || (orLists.get(0).size() == 1 && orLists.get(2).get(0) == 0)) {
                Organization org = organizationService.findByClientIdAndId(BussinessCommon.getClientId(), orgListIds.get(0));
                if (org != null) {
                    c += org.getName() + " , ";
                }
            }
        }
        comment = c + comment;


        if (comment != null) {
            DocumentComment cmt = new DocumentComment();
            cmt.setDocId(docId);
            cmt.setComment(comment);
            cmt.setType(type);
            cmt = cmtService.saveCmt(cmt);
            if (files != null && files.length > 0) {
                attachCmtService.addListAttachmentComment(files, cmt.getId());
            }
        }
    }

    public ListObjectDto<DocumentInReceiveDto> getWaitToReceive(String text, Boolean important, Boolean expired,
                                                                String numberOrSign, String preview, Long docTypeId, Long docFieldsId, Long methodReceiptId,
                                                                Pageable pageable) {
        User u = BussinessCommon.getUser();
        List<DocumentStatusEnum> status = Arrays.asList(DocumentStatusEnum.WAIT_RECEIVE);

        Date now = DateTimeUtils.handleSubmit(new Date());
        Page<DocumentInReceiveDto> rsPage = null;
        if (!StringUtils.isNullOrEmpty(text)) {
            rsPage = docRepository.findWaitToReceive(clericalOrg, DocumentTypeEnum.VAN_BAN_DEN, u.getId(), text, u.getOrg(), status,
                    u.getClientId(), pageable);
        } else {
            rsPage = docRepository.findWaitToReceive(clericalOrg, important, expired, now, DocumentTypeEnum.VAN_BAN_DEN, u.getId(),
                    numberOrSign, preview, docTypeId, docFieldsId, methodReceiptId, u.getOrg(), status, u.getClientId(),
                    pageable);
        }

        return BussinessCommon.paging(rsPage);
    }

    @Transactional
    public Boolean finish(Long docId, String comment, MultipartFile[] files) {
        User u = BussinessCommon.getUser();

        Documents doc = validDocId(docId);

        BussinessCommon.require("Ý kiến xử lý", comment);
        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);

        // Check process
        DocumentInProcess oProcess = processService.findByToUserAndDocId2(u.getId(), docId);

        if (oProcess == null
                || !DocumentInHandleStatusEnum.CHUYEN_DON_VI.equals(oProcess.getHandleStatus())) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }

        if (!checkCanFinish(doc, oProcess.getHandleStatus())) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }

        // update process
        oProcess.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);
        processService.save(oProcess);

        processService.closeDoc(docId, oProcess);

        // Delete notifications
        notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);

        // tracking
        trackingServices.save(docId, DocumentInTrackingEnum.FINISH, u.getId());

        // cmt
        saveCommentAndAttach(docId, files, comment);

        return true;
    }

    @Transactional
    public DocumentBasicDto reject(Long docId, String comment, MultipartFile[] files) {
        User u = BussinessCommon.getUser();
        Documents doc = validDocId(docId);
        if (DocumentStatusEnum.DONE.equals(doc.getStatus()) && doc.getParentId() == null) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }

        // Check process
        DocumentInProcess parent = processService.findByToUserAndDocId2(u.getId(), doc.getParentId());
        if (parent == null
                || !DocumentInHandleStatusEnum.CHUYEN_DON_VI.equals(parent.getHandleStatus())) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }

        // Trong đơn vị con thì có Main mới chuyển xử lý
        DocumentInProcess child = processService.findLastStepByDocIdAndHandleType(docId, HandleTypeEnum.MAIN);
        if (child == null || !DocumentInHandleStatusEnum.DA_XU_LY.equals(child.getHandleStatus())) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        // update doc status
        doc.setStatus(DocumentStatusEnum.RETURN_DOC);
        doc = docRepository.save(doc);

        // add for transfer
        child.setHandleStatus(DocumentInHandleStatusEnum.CHO_XU_LY);
        child.setProgress(0);
        processService.save(child);

        trackingServices.save(docId, DocumentInTrackingEnum.INCOMING, child.getToUser());

        //Add tracking
        DocumentInTrackingEnum trackingSts = DocumentInTrackingEnum.REJECT;
        if (u.getId().equals(parent.getDelegaterId())) {
            trackingSts = DocumentInTrackingEnum.REJECT_UQ;
        }
        trackingServices.add(
                new DocumentInTracking(docId, u.getId(), trackingSts, parent.getOrgName(), doc.getDocType().getName()));

        // Add notification
        notiService.add(child.getToUser(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                NotificationHandleStatusEnum.BI_TRA_LAI, ModuleCodeEnum.DOC_OUT_MAIN);

        // for delegate
        if (child.getDelegaterId() != null) {
            notiService.add(child.getDelegaterId(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                    NotificationHandleStatusEnum.BI_TRA_LAI_UQ, ModuleCodeEnum.DOC_IN_DELEGATE);
        }

        // comment
        saveCommentAndAttach(docId, files, comment);

        // Delete notifications
        notiService.setActiveByDocIdAndDocType(doc.getParentId(), DocumentTypeEnum.VAN_BAN_DEN, false);

        return new DocumentBasicDto(doc);
    }

    @Transactional
    public Boolean retakeOrgTransfer(Long docId, String comment, MultipartFile[] files) {
        User u = BussinessCommon.getUser();
        Documents doc = validDocId(docId);

        if (!DocumentStatusEnum.WAIT_RECEIVE.equals(doc.getStatus()) || doc.getParentId() == null) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        // Check process
        List<Long> usersCheck = getUserOrListVanThuBan();
        DocumentInProcess oProcess = processService.findByToUsersAndDocId2(usersCheck, doc.getParentId());
        if (oProcess == null
                || !DocumentInHandleStatusEnum.CHUYEN_DON_VI.equals(oProcess.getHandleStatus())) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        // Update doc status
        doc.setStatus(DocumentStatusEnum.RETAKE_ORG);
        docRepository.save(doc);
        // Update process status
        boolean children = false;
        for (Documents d : doc.getParent().getListChildren()) {
            if (!DocumentStatusEnum.RETAKE_ORG.equals(d.getStatus())
                    && !DocumentStatusEnum.REJECT_RECEIVE.equals(d.getStatus())) {
                children = true;
                break;
            }
        }
        if (!children) {
            oProcess.setHandleStatus(DocumentInHandleStatusEnum.DANG_XU_LY);
            processService.save(oProcess);
        }
        // comment
        saveCommentAndAttach(doc.getParentId(), files, comment);

        // Delete notifications
        notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);

        return true;
    }

    @Transactional
    public Boolean requestReview(Long docId, String comment, MultipartFile[] files, Boolean isDelegate) {
        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);
        User u = BussinessCommon.getUser();
        Documents doc = validDocId(docId);
        DocumentInProcess oProcess = processService.findByToUserAndDocId2(u.getId(), docId);
        if (oProcess == null || oProcess.getReview() != null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }

        boolean review = bpmnService.getReviewRequiredByNodeId(oProcess.getNode());
        if (!processService.canRequestReview(oProcess, review)) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }

        //transfer process
        DocumentInProcess tfProcess = processService.transferToUserId(docId, oProcess.getFrUser());
        if (tfProcess == null) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }

        notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);

        //transfer process
        tfProcess.setHandleStatus(DocumentInHandleStatusEnum.CHO_DANH_GIA);
        objReadService.setRead(tfProcess.getToUser(), docId, DocumentTypeEnum.VAN_BAN_DEN, false);
        processService.save(tfProcess);
        notiService.add(tfProcess.getToUser(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN, NotificationHandleStatusEnum.XIN_DG, ModuleCodeEnum.DOC_OUT_MAIN);
        if (tfProcess.getDelegaterId() != null) {
            notiService.add(tfProcess.getDelegaterId(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN, NotificationHandleStatusEnum.XIN_DG_UQ, ModuleCodeEnum.DOC_IN_DELEGATE);
        }

        //old process
        oProcess.setHandleStatus(DocumentInHandleStatusEnum.XIN_DANH_GIA);
        oProcess.setReview(false);
        objReadService.setRead(oProcess.getToUser(), docId, DocumentTypeEnum.VAN_BAN_DEN, false);
        processService.save(oProcess);

        //Add comment and attachment
        saveCommentAndAttach(docId, files, comment);
        return true;
    }

    @Transactional
    public Boolean review(Long docId, Long pId, boolean agree, String comment, MultipartFile[] files) {
        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);
        User u = BussinessCommon.getUser();
        Documents doc = validDocId(docId);

        // old proccess
        DocumentInProcess oProcess = processService.findByToUserAndDocId2(u.getId(), docId);
        if (oProcess == null || !processService.canReview(oProcess)) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }

        // request process
        DocumentInProcess rqProcess = processService.valid(pId, Message.INVALID_PROCESS);
        notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);

        // reviewer
        oProcess.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);

        // request person
        DocumentInHandleStatusEnum handleSts;
        NotificationHandleStatusEnum noticeSts;
        Boolean review = null;
        if (agree) {
            handleSts = DocumentInHandleStatusEnum.DG_CHAP_NHAN;
            noticeSts = NotificationHandleStatusEnum.DG_CHAP_NHAN;
            review = true;
        } else {
            noticeSts = NotificationHandleStatusEnum.DG_TU_CHOI;
            handleSts = DocumentInHandleStatusEnum.DG_TU_CHOI;
        }

        rqProcess.setHandleStatus(handleSts);
        rqProcess.setReview(review);
        processService.save(rqProcess);
        notiService.add(rqProcess.getToUser(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN, noticeSts,
                ModuleCodeEnum.DOC_OUT_MAIN);
        if (rqProcess.getDelegaterId() != null) {
            notiService.add(rqProcess.getDelegaterId(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                    noticeSts, ModuleCodeEnum.DOC_OUT_MAIN);
        }

        objReadService.setRead(rqProcess.getToUser(), docId, DocumentTypeEnum.VAN_BAN_DEN, false);

        // Add comment and attachment
        saveCommentAndAttach(docId, files, comment);
        return true;
    }

    @Transactional
    public void returnDoc3(Long docId, String comment, MultipartFile[] files, boolean isDelegate) {
        User u = BussinessCommon.getUser();
        DocumentInProcess oProcess = null;
        if (!encryptFiles && StringUtils.isNullOrEmpty(comment)) {
            throw new RestExceptionHandler(Message.REQUIRES_RETURN_DOC_REASON);
        }

        if (isDelegate) {
            oProcess = processService.findByDelegateAndDocId(u.getId(), docId, HandleTypeEnum.MAIN);
            if (oProcess == null || !processService.isType(oProcess.getHandleType(), HandleTypeEnum.MAIN)
                    || !processService.isStatus(oProcess.getHandleStatus(), processService.HANDLE_STATUS_NOT_YET)) {
                throw new RestExceptionHandler(Message.NO_DELEGATE);
            }
        } else {
            oProcess = processService.findByToUserAndDocId(u.getId(), docId);
            if (oProcess == null || !processService.isType(oProcess.getHandleType(), HandleTypeEnum.MAIN)
                    || !processService.isStatus(oProcess.getHandleStatus(), processService.HANDLE_STATUS_NOT_YET)) {
                throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
            }
        }

        List<DocumentInProcess> previousProcessList = processService.getTransferStep(docId);
        if (previousProcessList.isEmpty()) {
            throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
        }

        Long node = previousProcessList.get(0).getNode();
        Long preNode = previousProcessList.get(0).getPreNode();
        // update node and status
        Documents doc = processService.updateDocByStatus(docId, DocumentStatusEnum.RETURN_DOC, node, preNode);

        // add previous process and tracking
        List<Long> userIdList = previousProcessList.stream().map(DocumentInProcess::getToUser).collect(Collectors.toList());
        List<Long> delegateIdList = previousProcessList.stream().map(DocumentInProcess::getDelegaterId).collect(Collectors.toList());
        List<User> userList = previousProcessList.stream().map(DocumentInProcess::getToUsers).collect(Collectors.toList());

        Integer maxTransferStep = processService.getMaxTransferStep(docId);

        processService.save(previousProcessList, oProcess.getStep() + 1, maxTransferStep == null ? Constant.NO_STEP + 1 : maxTransferStep + 1);
        trackingServices.save(doc, DocumentInTrackingEnum.INCOMING, userList, null, Constant.OTHER_ACTION_TYPE);

        //update previous process list is RETURN
        processService.updateTransferStep(previousProcessList, Constant.NO_STEP);

        // notification
        notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);

        // update for old + update old process is DA_TRA_LAI_UQ
        List<DocumentInProcess> prList = processService.update(docId, oProcess.getStep(),
                DocumentInHandleStatusEnum.DA_XU_LY, u.getId(), Constant.RETURN_DOC_TYPE);

        // update old process is DA_TRA_LAI_UQ
        if (isDelegate) {
            processService.saveFrDelegate(oProcess, HandleTypeEnum.MAIN, DocumentInHandleStatusEnum.DA_TRA_LAI_UQ);
            trackingServices.save(docId, DocumentInTrackingEnum.REJECT_UQ, u.getId());
        }

        List<Long> userId = prList.stream().map(DocumentInProcess::getToUser).collect(Collectors.toList());
        List<User> userIds = userService.findByIds(userId, true);
        trackingServices.save(doc, DocumentInTrackingEnum.FINISH, userIds, u.getId(), Constant.RETURN_DOC_TYPE);

        // comment
        saveCommentAndAttach(docId, files, comment);

        // notification
        notiService.addAll(userIdList, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN, NotificationHandleStatusEnum.BI_TRA_LAI, ModuleCodeEnum.DOC_OUT_MAIN);

        // for delegate
        if (!delegateIdList.isEmpty()) {
            notiService.addAll(delegateIdList, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN, NotificationHandleStatusEnum.BI_TRA_LAI_UQ, ModuleCodeEnum.DOC_OUT_MAIN);
        }
    }

    public boolean requestComment(Long docId, Long[] orgIdArr, Long[] toUserIdArr, String comment, MultipartFile[] files) {
        try {
            BussinessCommon.validLengthData(comment, "Ý kiến", 2000);

            User user = BussinessCommon.getUser();

            Documents doc = validDocId(docId);

            if (BussinessCommon.isEmptyArr(orgIdArr) && BussinessCommon.isEmptyArr(toUserIdArr)) {
                throw new RestExceptionHandler(Message.NO_INPUT_DATA);
            }

            if (!processService.canAsk(docId, user.getId())) {
                throw new RestExceptionHandler(Message.NO_PROCESS_DOC);
            }

            List<User> toUsers = userService.getListUserByOrgIdsAndUserIds(orgIdArr, toUserIdArr);
            List<Long> toUserIds = toUsers.stream().map(User::getId).collect(Collectors.toList());

            //add manipulation
            manipulationService.save(docId, toUserIds, DocumentInHandleStatusEnum.CHO_CHO_Y_KIEN);

            //add tracking
            trackingServices.save(doc, DocumentInTrackingEnum.XIN_Y_KIEN, user);

            //add notification
            if (!toUserIds.isEmpty()) {
                notiService.addAll(toUserIds, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN, NotificationHandleStatusEnum.XIN_Y_KIEN, ModuleCodeEnum.DOC_OUT_MAIN, user.getId());
            }

            // comment
            saveCommentAndAttach(docId, files, comment, DocumentCommentTypeEnum.XIN_Y_KIEN);

            for (Long id : toUserIds) {
                objReadService.setRead(id, docId, DocumentTypeEnum.VAN_BAN_DEN, false);
            }

        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }

        return true;
    }

    @Transactional
    public boolean replyComment(Long docId, String comment, MultipartFile[] files) {

        try {
            BussinessCommon.validLengthData(comment, "Ý kiến", 2000);

            User user = BussinessCommon.getUser();

            Documents doc = validDocId(docId);

            //update manipulation
            List<Long> frUsers = manipulationService.update(docId, user.getId(), DocumentInHandleStatusEnum.DA_CHO_Y_KIEN);

            //add tracking
            trackingServices.save(doc, DocumentInTrackingEnum.DA_Y_KIEN, user);

            //del & add notification
            notiService.setActiveByUserIdAndDocIdAndDocType(user.getId(), docId, DocumentTypeEnum.VAN_BAN_DEN, false);
            notiService.setActiveByListUserIdAndDocIdAndDocType(frUsers, docId, DocumentTypeEnum.VAN_BAN_DEN, false);
            if (!frUsers.isEmpty()) {
                notiService.addAll(frUsers, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN, NotificationHandleStatusEnum.DA_Y_KIEN, ModuleCodeEnum.DOC_OUT_MAIN);
            }

            // comment
            saveCommentAndAttach(docId, files, comment);

        } catch (Exception e) {
            return false;
        }

        return true;
    }

    public List<Documents> switchOrAddUser(Long docId, String comment, String cmtContent, String[] main, String[] support, String[] show,
                                           Long node, MultipartFile[] files, Long[] orgMain, Long[] orgSupport, Long[] orgShow, Date deadline,
                                           boolean isMain, Long[] direction) {
        DocumentInHandleStatusEnum[] done = {DocumentInHandleStatusEnum.DA_XU_LY, DocumentInHandleStatusEnum.DA_XU_LY_UQ};
        Documents doc = validDocId(docId);
        HandleTypeEnum handleTypeEnum = getDocumentHandleType(doc);

        User u = BussinessCommon.getUser();

        all = new HashSet<>();
        List<User> userList = getUserByArr(main, support, show, direction);
        List<HandlerDto> uMain = add(orgMain, main, userList, true);
        List<HandlerDto> uSupports = add(orgSupport, support, userList, false);
        List<HandlerDto> uShows = add(orgShow, show, userList, false);
        List<HandlerDto> uDirections = !BussinessCommon.isEmptyArr(direction) ? getHandleDto(direction, userList) : new ArrayList<>();

        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);
        if (node == 0L || node == null) {
            return Collections.emptyList();
        }

        DocumentInProcess oProcess = processService.findByToUserAndDocIdAndNode(node, u.getId(), docId);
        if (oProcess == null || isMain && !processService.isType(oProcess.getHandleType(), HandleTypeEnum.MAIN)) {
            throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
        }

        Integer steps = oProcess.getStep();
        Long preNode = oProcess.getPreNode();
        Integer transferStep = oProcess.getTransferStep();
        User frUser = oProcess.getFrUsers();
        node = oProcess.getNode();
        if (deadline == null) {
            deadline = oProcess.getDeadline();
        }

        boolean isSwitch =
//				!DocumentInHandleStatusEnum.DA_XU_LY.equals(oProcess.getHandleStatus())
//				&& !DocumentInHandleStatusEnum.DA_XU_LY_UQ.equals(oProcess.getHandleStatus()) &&
                isMain
                        && !BussinessCommon.isEmptyList(uMain);

        DocumentInHandleStatusEnum statusAdd = processService.isStatusInArray(done, oProcess.getHandleStatus()) ? oProcess.getHandleStatus() : DocumentInHandleStatusEnum.DA_XU_LY_ADD_USER;
        if (!BussinessCommon.isEmptyList(uSupports)) {
            if (!isSwitch) {
                processService.update(oProcess, statusAdd);
            }
            trackingServices.save(docId, DocumentInTrackingEnum.ADD_USER_HANDLE, u.getId());
        }

        DocumentInHandleStatusEnum statusSwitch = DocumentInHandleStatusEnum.DA_XU_LY_SWITCH;
        if (isSwitch) {
            // update process for old
            processService.update(docId, steps, statusSwitch, HandleTypeEnum.MAIN);
            // update tracking for old
            trackingServices.save(docId, DocumentInTrackingEnum.SWITCH, u.getId());
        }

        // add process + tracking
        addProcesss(frUser, preNode, node, doc, steps, uMain, uSupports, uShows, uDirections, comment, cmtContent, DocumentCommentTypeEnum.THEM_XU_LY, files, deadline, transferStep, false, false, handleTypeEnum);
        List<Documents> docList = new ArrayList<>();
        docList.add(doc);
        this.all.clear();
        return docList;
    }

    public boolean retakeDone(Long docId, String comment, MultipartFile[] files) {
        Documents doc = validDocId(docId);
        Long userId = BussinessCommon.getUserId();

        //update old process
        DocumentInProcess oProcess = processService.findByToUserAndDocId2(userId, docId);
        if (oProcess == null || !processService.canRetakeDone(oProcess, doc)) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }

        oProcess.setHandleStatus(DocumentInHandleStatusEnum.THU_HOI_HOAN_THANH);
        oProcess.setEndTask(null);
        oProcess.setCloseBranch(null);
        processService.save(oProcess);

        //tracking
        DocumentInTrackingEnum trackingSts = DocumentInTrackingEnum.RETAKE_DONE;
        if (userId.equals(oProcess.getDelegaterId())) {
            trackingSts = DocumentInTrackingEnum.RETAKE_DONE_UQ;
        }
        trackingServices.add(
                new DocumentInTracking(docId, userId, trackingSts, oProcess.getOrgName(), doc.getDocType().getName()));

        // update doc status
        if (DocumentStatusEnum.DONE.equals(doc.getStatus())) {
            doc.setStatus(DocumentStatusEnum.DOING);
            docRepository.save(doc);
        }

        //save comment
        saveCommentAndAttach(docId, files, comment);

        return true;
    }

    public List<Documents> getByRemind() {
        User u = BussinessCommon.getUser();
        boolean isLibrarian = userService.isVanThuVBDen(u);
        List<Long> userIds = getListUserId(isLibrarian);
        return docRepository.findByUserIdAndClientId(clericalOrg, u.getId(), DocumentStatusEnum.RETAKE_DOC,
                DocumentStatusEnum.NOT_YET, isLibrarian, userIds, BussinessCommon.getClientId());
    }

    public List<Documents> findByIds(List<Long> ids) {
        return docRepository.findByClientIdAndActiveTrueAndIdIn(BussinessCommon.getClientId(), ids);
    }

    @SuppressWarnings("unchecked")
    public Attachment saveResolveFile(Documents doc) {
        ResolvedDocumentDto dto = new ResolvedDocumentDto(doc);
        Organization orgModel = BussinessCommon.getUser().getOrgModel();
        dto.setOrgName(orgModel != null ? orgModel.getName() : "");
        dto.setOrgNameUpper(orgModel != null ? orgModel.getName().toUpperCase() : "");
        if (orgModel != null) {
            String text = "";
            if (orgModel.getOrgConfigSign() != null) {
                User user = userService.findByUserId(orgModel.getOrgConfigSign().getUserId());
                if (user != null) {
                    text = user.getPositionModel().getName();
                }
            }
            if (orgModel.getLevel() == 0) {
                dto.setPosition(text + ": " + dto.getOrgName());
                String signer = Constant.CHANH_VAN_PHONG.replaceFirst("C", "c");
                dto.setSinger(signer);
                dto.setSingerUpper(signer.toUpperCase());
            } else if (orgModel.getLevel() == 1 || orgModel.getLevel() == 2) {
                dto.setPosition(text + ": " + dto.getOrgName());
                String signer = "Trưởng phòng hành chính";
                dto.setSinger(signer);
                dto.setSingerUpper(signer.toUpperCase());
            } else dto.setPosition("");
        }
        if (Constant.ORG_NAME_UPPER.equals(dto.getOrgNameUpper())) {
            dto.setOrgNameUpper("");
        }
        DocxStamperConfiguration stamperConfig = new DocxStamperConfiguration();
        DocxStamper<ResolvedDocumentDto> stamper = stamperConfig.build();
        OutputStream outputStream = null;
        try {
            Category category = categoryService.findByClientIdAndId(BussinessCommon.getClientId(), doc.getPlaceSendId());
            dto.setPlaceSendName(category != null ? category.getName() : "");
            File fs = ResourceUtils.getFile("classpath:templates/Phiếu trình xử lý văn bản đến.docx");
            String nameParse = FilesStorageService.parse(fs.getName());
            File nFile = saveToSystem(fs, nameParse, rootPath);
            outputStream = new FileOutputStream(nFile);
            stamper.stamp(new FileInputStream(fs), dto, outputStream);
            if (ftp) {
                String dir = fileService.getDir(rootPath);
                InputStream inputStream = new FileInputStream(nFile);
                ftpService.upload(nameParse, dir, inputStream);
                inputStream.close();
                // nFile is tmp file -> remove
                Path file = rootPath.resolve(nFile.getName());
                Files.delete(file);
            }
            return attService.save(doc.getId(), nFile);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            try {
                if (outputStream != null)
                    outputStream.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        throw new RestExceptionHandler(Message.ERROR_SYS);
    }

    private File saveToSystem(File oFile, String fileName, Path rootPath) {
        Path path = rootPath.resolve(fileName);
        try (FileInputStream inputStream = new FileInputStream(oFile)) {
            Files.copy(inputStream, path, StandardCopyOption.REPLACE_EXISTING);
            Resource resource = new UrlResource(path.toUri());
            File nFile = resource.getFile();
            FileUtils.copyFile(oFile, nFile);
            return nFile;
        } catch (IOException e) {
            e.printStackTrace();
        }
        throw new RestExceptionHandler(Message.ERROR_SYS);
    }

    /**
     * step process
     * 1. del old file
     * 2. copy template
     * 3. get data comments print to template file
     *
     * @param doc
     * @return
     */
    @SuppressWarnings("unchecked")
    public Attachment updateResolveFile(Documents doc) {
        List<HandleTypeEnum> typePermit = Arrays.asList(HandleTypeEnum.SUPPORT, HandleTypeEnum.SHOW);
        Long userId = BussinessCommon.getUserId();
        boolean isLeaderShip = authorityService.isUserHasAuthority(userId, null, AuthorityEnum.LEADERSHIP);
        DocumentInProcess oProcess = processService.findByToUserAndDocId(userId, doc.getId());
        if (oProcess == null || (!typePermit.contains(oProcess.getHandleType()) && !isLeaderShip)) {
            return new Attachment();
        }

        Attachment att = attService.getByDocIdAndType(doc.getId());
        if (att == null) {
            att = saveResolveFile(doc);
        }
        OutputStream outputStream = null;
        try {
            // del old file
            fileService.delete(att.getName());
            DocxStamperConfiguration stamperConfig = new DocxStamperConfiguration();
            DocxStamper<ResolvedDocumentDto> stamper = stamperConfig.leaveEmptyOnExpressionError(true).build();
            ResolvedDocumentDto dto = new ResolvedDocumentDto(doc, getCommentsByDocId(doc.getId()));
            Organization orgModel = organizationService.getOrgCucByOrg(BussinessCommon.getUser().getOrgModel());
            dto.setOrgName(orgModel != null ? orgModel.getName() : "");
            dto.setOrgNameUpper(orgModel != null ? orgModel.getName().toUpperCase() : "");
            if (orgModel != null) {
                String text = orgModel.getOrgConfigSign() != null ? orgModel.getOrgConfigSign().getUser().getPositionModel().getName() : "";
                if (orgModel.getLevel() == 0) {
                    dto.setPosition(text + ": " + dto.getOrgName());
                    dto.setSinger(text);
                } else if (orgModel.getLevel() == 1) {
                    dto.setPosition(text + ": " + dto.getOrgName());
                    dto.setSinger("Trưởng phòng hành chính");
                } else dto.setPosition("");
            }
            File fs = ResourceUtils.getFile("classpath:templates/Phiếu trình xử lý văn bản đến.docx");
            String nameParse = att.getName();
            File nFile = saveToSystem(fs, nameParse, rootPath);
            outputStream = new FileOutputStream(nFile);
            stamper.stamp(new FileInputStream(fs), dto, outputStream);

            if (ftp) {
                String dir = fileService.getDir(rootPath);
                InputStream inputStream = new FileInputStream(nFile);
                ftpService.upload(nameParse, dir, inputStream);
                inputStream.close();
                // nFile is tmp file -> remove
                Path file = rootPath.resolve(nFile.getName());
                Files.delete(file);
            }

            // update file name of attachment
            attService.update(att, nFile);
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            StreamUtils.closeOutputStream(outputStream);
        }
        return att;
    }

    private List<ResolvedUserDto> getCommentsByDocId(Long docId) {
        List<ResolvedUserDto> dtos = getUserInfoByDocId(docId);
        List<Long> userIds = dtos.stream().map(ResolvedUserDto::getId).collect(Collectors.toList());
        List<DocumentComment> cmts = cmtService.getByDocId(docId, userIds);
        return ResolvedUserDto.convert(cmts, dtos);
    }

    public List<ResolvedUserDto> getUserInfoByDocId(Long docId) {
        return docRepository.getUserInfoByDocId(docId, BussinessCommon.getClientId());
    }

    private Documents getDocInByDocOutAndOrgId(DocumentOut docOut, Long orgId) {
        return docRepository.getDocInByDocOutAndOrgId(docOut.getBookId(), docOut.getNumberInBook(), orgId, docOut.getClientId());
    }

    private Organization getOrgByCondition(Long receiveId, String type) {
        Organization org;
        if (Constant.RECEIVER_TYPE_USER.equals(type)) {
            User user = userService.valid(receiveId, Message.ERROR_SYS);
            org = user.getOrgModel();
        } else if (Constant.RECEIVER_TYPE_ORG.equals(type)) {
            org = orgService.valid(receiveId, Message.ERROR_SYS);
        } else {
            return null;
        }

        return org;
    }

    public void saveFromDocOut(DocumentOut docOut, Long receiveId, String type) {
        // Target: Save document and process corresponding
        // Step 1: Document in is created from document out
        // Step 2: Add process and tracking corresponding user
        // Step 3: Add attachment in document in

        boolean personalReceive = Constant.RECEIVER_TYPE_USER.equals(type);

        if (personalReceive)
            return;

        Organization org = getOrgByCondition(receiveId, type);

        if (org == null) {
            return;
        }

        Long orgId = org.getId();
        Documents doc = getDocInByDocOutAndOrgId(docOut, orgId);
        if (doc != null)
            return;

        List<User> uList;
        if (Boolean.TRUE.equals(org.getIsDefault())
                || Constant.PHONG.equalsIgnoreCase(org.getOrgTypeModel().getName())) { // Văn phòng thuộc BAN
            if (this.docInternalRemove)
                return;

            uList = userService.getListLeadUserIdByOrg(orgId);
            doc = save(docOut, orgId, DocumentStatusEnum.DOING);
            for (User user : uList) {
                processService.save(Constant.START_NODE, doc.getId(), DocumentInHandleStatusEnum.CHO_XU_LY,
                        HandleTypeEnum.MAIN, Constant.START_STEP, 0, user, null, null, null, null);
                trackingServices.save(doc.getId(), DocumentInTrackingEnum.INCOMING, user.getId());
                notiService.add(user.getId(), doc.getId(), doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                        NotificationHandleStatusEnum.MOI_DEN, ModuleCodeEnum.DOCUMENT_OUT_INTERNAL);
            }
        } else {

            if (clericalOrg) {
                uList = clericalOrgService.getClericalOrgUserByOrgId(orgId);
            } else {
                uList = rService.getListVanThuVBDenByOrg(orgId);
            }

            // Văn bản ban hành không được đưa về chính tổ chức ban hành
            boolean dupplicate = uList.stream().filter(i -> BussinessCommon.getUserId().equals(i.getId()))
                    .count() > 0;
            if (dupplicate) return;

            doc = save(docOut, orgId, DocumentStatusEnum.WAIT_RECEIVE);
            for (User user : uList) {
                processService.save(Constant.START_NODE, doc.getId(), DocumentInHandleStatusEnum.MOI_NHAN,
                        HandleTypeEnum.MAIN, Constant.START_STEP, Constant.START_STEP, user, null, user.getId(),
                        doc.getDeadline(), null);
                trackingServices.save(doc.getId(), DocumentInTrackingEnum.RECEIVE, user.getId());

                notiService.add(user.getId(), doc.getId(), doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                        NotificationHandleStatusEnum.CHO_TIEP_NHAN, ModuleCodeEnum.DOC_OUT_LIST);
            }
        }

        attService.cloneFile(docOut, doc.getId());
        saveResolveFile(doc);
    }

    private Documents save(DocumentOut docOut, Long orgId, DocumentStatusEnum status) {
        String placeSendName = BussinessCommon.getUser().getOrgModel().getName();
        Documents doc = new Documents(docOut, orgId, status);
        Category methodReceipt = categoryService.findByCategoryCodeAndDefault(Constant.CAT_TYPE_RECEIVE);
        if (methodReceipt != null)
            doc.setMethodReceiptId(methodReceipt.getId());

        Category placeSend = categoryService.findByNameAndCategoryCode(placeSendName, Constant.CAT_ORG_OTHER);
        if (placeSend == null) {
            placeSend = categoryService.save(placeSendName, Constant.CAT_ORG_OTHER, "Nơi nhận bên ngoài");
        }
        doc.setPlaceSendId(placeSend.getId());
        doc.setDocOutId(docOut.getId());
        return docRepository.save(doc);
    }

    private void handleByConfidentialDoc2Receiver(List<Documents> parents, Long nodeId, List<User> users) {
        if (nodeId == null || parents == null || parents.isEmpty() || users == null || users.isEmpty())
            return;

        Map<Long, Long> uMap = new HashMap<>();
        Map<Long, Long> pMap = new HashMap<>();
        users.forEach(i -> {
            uMap.put(i.getId(), i.getOrg());
            pMap.put(i.getPosition(), i.getOrg());
        });
        List<Condition> cs = bpmnService.getConditionByNodeIdAndSecurityTrue(nodeId);
        Set<Long> orgIds = new HashSet<>();
        for (Condition c : cs) {
            Long uKey = c.getUserId();
            Long pKey = c.getPositionId();
            if (uKey != null && uMap.containsKey(uKey)) {
                orgIds.add(uMap.get(uKey));
                continue;
            }
            if (pKey != null && pMap.containsKey(pKey)) {
                orgIds.add(pMap.get(pKey));
                continue;
            }
        }

        for (Long i : orgIds) {
            Documents tmp = null;
            // Add notification for clerical
            List<User> uList = new ArrayList<>();
            if (clericalOrg) {
                uList = clericalOrgService.getClericalOrgUserByOrgId(i);
            } else {
                uList = rService.getListVanThuVBDenByOrg(i);
            }

            for (Documents j : parents) {
                // Find document exist by parent doc
                tmp = docRepository.findFirstByClientIdAndParentIdAndOrgReceiveIdAndActiveTrue(
                        BussinessCommon.getClientId(), j.getId(), i);
                if (tmp != null) {
                    continue;
                }
                tmp = new Documents(j, nodeId, j.getId(), DocumentStatusEnum.WAIT_RECEIVE, i, true);
                tmp = docRepository.save(tmp);

                for (User user : uList) {
                    processService.save(Constant.START_NODE, tmp.getId(), DocumentInHandleStatusEnum.MOI_NHAN,
                            HandleTypeEnum.MAIN, Constant.START_STEP, Constant.START_STEP, user, null, user.getId(),
                            tmp.getDeadline(), null);
                    trackingServices.save(tmp.getId(), DocumentInTrackingEnum.RECEIVE, user.getId());
                    notiService.add(user.getId(), tmp.getId(), "***Văn bản mật***", DocumentTypeEnum.VAN_BAN_DEN,
                            NotificationHandleStatusEnum.CHO_TIEP_NHAN, ModuleCodeEnum.DOC_OUT_LIST);
                }
            }
        }
    }

    public List<Documents> transfer2(Long docId, String comment, String cmtContent, String[] main, String[] support, String[] show,
                                     Long node, MultipartFile[] files, Long[] orgMain, Long[] orgSupport, Long[] orgShow,
                                     Date deadline, Long[] direction, Boolean requestReview) {
        Documents doc = validDocId(docId);

        HandleTypeEnum handleTypeEnum = getDocumentHandleType(doc);

        User u = BussinessCommon.getUser();
        this.all = new HashSet<>();
        if (this.mainRequire && ArrayUtils.isEmpty(main) && ArrayUtils.isEmpty(orgMain) && ArrayUtils.isEmpty(direction))
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);

        boolean hasMain = !ArrayUtils.isEmpty(main) || !ArrayUtils.isEmpty(orgMain);
        List<User> userList = getUserByArr(main, support, show, direction);
        List<HandlerDto> uMain = hasMain ? add(orgMain, main, userList, true) : getHandleDto(direction, userList);
        List<HandlerDto> uSupports = add(orgSupport, support, userList, false);
        List<HandlerDto> uShows = add(orgShow, show, userList, false);
        List<HandlerDto> uDirections = !BussinessCommon.isEmptyArr(direction) && hasMain ? getHandleDto(direction, userList) : new ArrayList<>();
        Long preNode = doc.getNode();
        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);
//        BussinessCommon.validLengthData(cmtContent, "Ý kiến xử lý", 2000);
        DocumentInProcess oProcess = processService.findByToUserAndDocId2(u.getId(), docId);
        boolean isDelegate = oProcess != null && u.getId().equals(oProcess.getDelegaterId());
        if (oProcess == null
                || !processService.isStatus(oProcess.getHandleStatus(), processService.HANDLE_STATUS_NOT_YET)) {
            boolean isClerical = userService.isVanThuVBDenByOrg(u, doc.getPersonEnter().getOrg());
            if (!isClerical && (oProcess != null && !DocumentInHandleStatusEnum.CHUYEN_DON_VI.equals(oProcess.getHandleStatus()))) {
                throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
            }

            oProcess = processService.findByDocIdAndFirstStep(docId);
            if (oProcess == null) {
                throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
            }
            oProcess.setToUser(u.getId());
        }

        //Check review required
        processService.validateReviewRequired(oProcess);

        // update old process
        oProcess.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);
        oProcess.setTransferStep(1);
        oProcess.setTransfer(true);
        oProcess.setNextNode(node);
        oProcess.setEndTask(null);
        oProcess.setCloseBranch(null);

        List<DocumentInHandleStatusEnum> notYetHandledStatus = Arrays.asList(DocumentInHandleStatusEnum.CHO_XU_LY, DocumentInHandleStatusEnum.DANG_XU_LY);

        if (handleTypeEnum.equals(HandleTypeEnum.INTERNAL_INCOMING)) {
            List<DocumentInProcess> processesWithSameStep = processService.findHandlesInSameStepExclusive(oProcess.getFrUser(), oProcess.getToUser(), oProcess.getDocId(), oProcess.getStep(), notYetHandledStatus);
            if (!processesWithSameStep.isEmpty()) {
                for (DocumentInProcess dip : processesWithSameStep) {
                    dip.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);
                    trackingServices.save(doc.getId(), DocumentInTrackingEnum.FINISH, dip.getToUser());
                }
                processService.getRepository().saveAll(processesWithSameStep);
            }
        }

        trackingServices.save(doc.getId(), DocumentInTrackingEnum.FINISH, u.getId());

        if (isDelegate) {
            u = oProcess.getToUsers();
            trackingServices.save(docId, DocumentInTrackingEnum.TRANSFER_UQ, u.getId());
        }

        // update doc status
        doc.setStatus(DocumentStatusEnum.DOING);
        docRepository.save(doc);

        if (deadline == null) {
            deadline = oProcess.getDeadline();
        }

        List<Documents> docList = new ArrayList<>();
        docList.add(doc);
        this.handleByConfidentialDoc2Receiver(docList, node, userList);

        // add process, tracking
        addProcesss(u, preNode, node, doc, oProcess.getStep() + 1, uMain, uSupports, uShows, uDirections, comment, cmtContent, DocumentCommentTypeEnum.CHUYEN_XU_LY, files, deadline, null, !hasMain, requestReview, handleTypeEnum);
        this.all.clear();
        return docList;
    }

    private HandleTypeEnum getDocumentHandleType(Documents doc) {
        HandleTypeEnum handleTypeEnum = HandleTypeEnum.MAIN;

        if (doc.getDocOutId() != null) {
            DocumentOut documentOut = docOutService.findByDocId(doc.getDocOutId());
            if (documentOut.getIsInternalDocument()) {
                handleTypeEnum = HandleTypeEnum.INTERNAL_INCOMING;
            } else {
                handleTypeEnum = HandleTypeEnum.INTERNAL_ISSUED_INCOMING;
            }
        }
        return handleTypeEnum;
    }

    public Boolean rejectByNodeId(Long pId, Long docId, String comment, MultipartFile[] files) {
        Long userId = BussinessCommon.getUserId();
//		if (StringUtils.isNullOrEmpty(comment)) {
//			throw new RestExceptionHandler(Message.REQUIRES_RETURN_DOC_REASON);
//		}

        Documents doc = valid(docId, Message.NOT_FOUND_OBJECT);

        DocumentInProcess oProcess = processService.findByToUserAndDocId2(userId, docId);
        if (oProcess == null || !processService.isStatus(oProcess.getHandleStatus(), processService.HANDLE_STATUS_RETURN)) {
            throw new RestExceptionHandler(Message.NO_PROCESS_WHEN_DONE_DOC);
        }

        DocumentInProcess pointReturn = processService.valid(pId, Message.NO_PROCESS_WHEN_DONE_DOC);
        if (!pointReturn.getDocId().equals(docId))
            throw new RestExceptionHandler(Message.NO_PROCESS_WHEN_DONE_DOC);

        // update old process, tracking
        oProcess.setHandleStatus(DocumentInHandleStatusEnum.DA_TRA_LAI);
        oProcess.setTransferStep(0);
        processService.save(oProcess);

        DocumentInTrackingEnum trackingSts = DocumentInTrackingEnum.REJECT;
        if (userId.equals(oProcess.getDelegaterId())) {
            trackingSts = DocumentInTrackingEnum.REJECT_UQ;
        }
        trackingServices.add(
                new DocumentInTracking(docId, userId, trackingSts, oProcess.getOrgName(), doc.getDocType().getName()));

        // add point return process, tracking, notification
        // even though delegate that from_user is toUser
        processService.reOpenProcess(pointReturn);
        objReadService.setRead(pointReturn.getToUser(), docId, DocumentTypeEnum.VAN_BAN_DEN, false);
        trackingServices.add(new DocumentInTracking(docId, pointReturn.getToUser(), DocumentInTrackingEnum.INCOMING,
                pointReturn.getOrgName(), doc.getDocType().getName()));
        notiService.add(pointReturn.getToUser(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                NotificationHandleStatusEnum.BI_TRA_LAI, getModule(pointReturn.getHandleType()), userId);
        if (pointReturn.getDelegaterId() != null) {
            notiService.add(pointReturn.getToUser(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                    NotificationHandleStatusEnum.BI_TRA_LAI_UQ, getModule(pointReturn.getHandleType()), userId);
        }

        // comment & files
        saveCommentAndAttach(docId, files, comment, DocumentCommentTypeEnum.TRA_LAI);
        return true;
    }

    @Transactional
    public boolean retakeByNodeId(Long docId, String comment, MultipartFile[] files) {
        // Target
        // 1. disable processes that are from the current process onwards
        // 2. re-open current process
        Documents doc = valid(docId, Message.NOT_FOUND_OBJECT);

        Long userId = BussinessCommon.getUserId();
        DocumentInProcess oProcess = processService.findByToUserAndDocId2(userId, docId);
        if (oProcess == null || !processService.canRetake(oProcess)) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }

        // inactive
        List<DocumentInProcess> parentAndChilds = processService.userTransferTo(oProcess, docId);
        parentAndChilds.remove(oProcess);
        processService.inactive(parentAndChilds);
        processService.updateDocByStatus(docId, DocumentStatusEnum.RETAKE_DOC);
        setStatusAndParentId(docId, DocumentStatusEnum.RETAKE_DOC);
        // del-add notification
        List<Long> uIdNotYetList = parentAndChilds.stream().filter(
                        i -> processService.isStatus(i.getHandleStatus(), DocumentInProcessService.HANDLE_STATUS_NOT_YET))
                .map(DocumentInProcess::getToUser).collect(Collectors.toList());

        List<Long> uIdDoneList = parentAndChilds.stream().filter(
                        i -> !processService.isStatus(i.getHandleStatus(), DocumentInProcessService.HANDLE_STATUS_NOT_YET))
                .map(DocumentInProcess::getToUser).collect(Collectors.toList());

        List<Long> uIdAllList = new ArrayList<>();
        uIdAllList.addAll(uIdNotYetList);
        uIdAllList.addAll(uIdDoneList);
        notiService.setActiveByListUserIdAndDocIdAndDocType(uIdAllList, docId, DocumentTypeEnum.VAN_BAN_DEN, false);
        notiService.addAll(uIdDoneList, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                NotificationHandleStatusEnum.DA_THU_HOI, ModuleCodeEnum.DOC_OUT_MAIN);

        // update old process, tracking
        oProcess.setHandleStatus(DocumentInHandleStatusEnum.DANG_XU_LY);
        oProcess.setTransferStep(0);
        oProcess.setEndTask(null);
        processService.save(oProcess);

        //Add tracking
        DocumentInTrackingEnum trackingSts = DocumentInTrackingEnum.RETAKE;
        if (userId.equals(oProcess.getDelegaterId())) {
            trackingSts = DocumentInTrackingEnum.RETAKE_UQ;
        }
        trackingServices.add(
                new DocumentInTracking(docId, userId, trackingSts, oProcess.getOrgName(), doc.getDocType().getName()));
        // comment
        saveCommentAndAttach(docId, files, comment);
        return true;
    }

    public boolean orgTransfer2(List<Long> docIdList, String comment, List<List<Long>> orgInputs, Long node,
                                MultipartFile[] files) {
        User user = BussinessCommon.getUser();
        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);
        if (BussinessCommon.isEmptyList(orgInputs)) {
            throw new RestExceptionHandler(Message.NO_INPUT_DATA);
        }
        boolean addOrg = false;
        for (Long docId : docIdList) {
            Documents doc = validDocId(docId);
            List<Long> usersCheck = getUserOrListVanThuBan();
            DocumentInProcess oProcess = processService.findByToUsersAndDocId2(usersCheck, docId);

            if (oProcess == null || !DocumentInProcessService.NOT_YET_ALL.contains(oProcess.getHandleStatus())) {
                throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE_PROCESS);
            }

            objReadService.setRead(oProcess.getToUser(), docId, DocumentTypeEnum.VAN_BAN_DEN, true);
            addOrg = DocumentInHandleStatusEnum.CHUYEN_DON_VI.equals(oProcess.getHandleStatus());
            if (!addOrg) {
                oProcess.setHandleStatus(DocumentInHandleStatusEnum.CHUYEN_DON_VI);
                processService.save(oProcess);

                DocumentInTrackingEnum trackingSts = DocumentInTrackingEnum.RETAKE;
                if (user.getId().equals(oProcess.getDelegaterId())) {
                    trackingSts = DocumentInTrackingEnum.FINISH;
                }
                trackingServices.add(new DocumentInTracking(docId, user.getId(), trackingSts, oProcess.getOrgName(),
                        doc.getDocType().getName()));
            } else {
                orgInputs = getOrgNew(doc, orgInputs);
            }

            saveCommentChuyenDonVi(docId, files, comment, DocumentCommentTypeEnum.CHUYEN_DON_VI, orgInputs);
            List<Documents> dList = docRepository.saveAll(saveDocsByOrgIdsorgTransfer2(doc, node, orgInputs));
//			attService.addListAttachment(doc.getAttachments(), dList);
            for (Documents d : dList) {
                List<Long> userIds = clericalOrg ? clericalOrgService.getClericalOrgByOrgId(d.getOrgReceiveId())
                        : userService.getListIdsVanThuVBDenByOrg(d.getOrgReceiveId());
                notiService.addAll(userIds, d.getId(), doc.getPreview(), DocumentTypeEnum.VAN_BAN_DEN,
                        NotificationHandleStatusEnum.CHO_TIEP_NHAN, ModuleCodeEnum.DOC_OUT_LIST);
            }
            trackingServices.save(dList, DocumentInTrackingEnum.TRANSFER, user);
        }
        return true;
    }

    private List<Long> getUserOrListVanThuBan() {
        User user = BussinessCommon.getUser();
        Category positionVanThuBan = categoryService.findByName(user.getClientId(), Constant.VAN_THU_MAIN);
        List<Long> usersCheck = new ArrayList<>();
        if (positionVanThuBan != null && user.getPositionModel() != null
                && (user.getPositionModel().getId().longValue() == positionVanThuBan.getId().longValue())) {
            usersCheck = userRepository.getUsersByNameCategory(Constant.VAN_THU_MAIN, user.getOrg());
            if (usersCheck.isEmpty()) {
                throw new RestExceptionHandler(Message.EMPTY_CLERICAL_HANDLE);
            }
        } else {
            usersCheck.add(user.getId());
        }
        return usersCheck;
    }

    private List<List<Long>> getOrgNew(Documents doc, List<List<Long>> orgInput) {
        List<List<Long>> rs = new ArrayList<>();
//		if (BussinessCommon.isEmptyList(orgInput)) {
//			return rs;
//		}

        Map<Long, Documents> map = new HashMap<>();
        for (Documents child : doc.getListChildren()) {
            Long key = child.getOrgReceiveId();
            if (key == null)
                continue;
            map.put(key, child);
        }

        orgInput.forEach(i -> {
            if (!map.containsKey(i.get(0)) || DocumentStatusEnum.RETAKE_ORG.equals(map.get(i.get(0)).getStatus())
                    || DocumentStatusEnum.REJECT_RECEIVE.equals(map.get(i.get(0)).getStatus())) {
                rs.add(i);
            }
        });
        return rs;
    }

    private ModuleCodeEnum getModule(HandleTypeEnum type) {
        switch (type) {
            case MAIN:
                return ModuleCodeEnum.DOC_OUT_MAIN;
            case SUPPORT:
                return ModuleCodeEnum.DOC_OUT_COMBINE;
            case SHOW:
                return ModuleCodeEnum.DOC_OUT_KNOW;
            case DIRECTION:
                return ModuleCodeEnum.DOC_OUT_DIRECTION;
            default:
                return ModuleCodeEnum.DOC_OUT_MAIN;
        }
    }

    public List<Object> getTransferedOrg(Long docId) {
        Documents doc = valid(docId, Message.NOT_FOUND_DOC);

        if (BussinessCommon.isEmptyList(doc.getListChildren()))
            return new ArrayList<>();

        List<Long> orgIds = new ArrayList<>();
        ArrayList<Object> listData = new ArrayList<Object>();
        ObjectMapper objectMapper = new ObjectMapper();
        doc.getListChildren().forEach(i -> {
            if (!DocumentStatusEnum.RETAKE_ORG.equals(i.getStatus())
                    && !DocumentStatusEnum.REJECT_RECEIVE.equals(i.getStatus())) {
                orgIds.add(i.getOrgReceiveId());
                ObjectNode node = objectMapper.createObjectNode();
                node.put("orgReceiveId", i.getOrgReceiveId());
                node.put("typeOrg", i.getTypeOrg());
                node.put("userReceiveId", i.getUserReceiveId());
                listData.add(node);
            }
        });
        return listData;
    }

    @Transactional
    public Boolean rejectReceiveDoc(Long docId, String comment, MultipartFile[] files) {
        // Valid data
        User u = BussinessCommon.getUser();
        if (!userService.isVanThuVBDen(u)) {
            throw new RestExceptionHandler(Message.NO_CREATE_DOC);
        }

        Documents d = valid(docId, Message.NOT_FOUND_DOC);
        boolean frDocOut = d.getParentId() == null;
        Long parentId = frDocOut ? d.getDocOutId() : d.getParentId();
        List<Long> userIds = new ArrayList<>();
        ModuleCodeEnum module = frDocOut ? ModuleCodeEnum.DRAFT_ISSUED : ModuleCodeEnum.DOC_OUT_MAIN;
        DocumentTypeEnum type = frDocOut ? DocumentTypeEnum.VAN_BAN_DI : DocumentTypeEnum.VAN_BAN_DEN;

        // Tracking
        if (frDocOut) {
            docOutTrackingService.save(parentId, DocumentOutTrackingEnum.REJECT_RECEIVE);
            userIds = clericalOrg ? clericalOrgService.getClericalOrgByOrgId(d.getOrgTransferId())
                    : userService.getListIdsVanThuVBDenByOrg(d.getOrgTransferId());
        } else {
            trackingServices.save(parentId, DocumentInTrackingEnum.REJECT_RECEIVE, u.getId());

            // Handle process of transfer of parent doc
            DocumentInProcess p = processService.getTransferParentDoc(parentId, d.getOrgTransferId());
            if (p != null) {
                userIds.add(p.getToUser());
            }
        }

        // Inactive noti
        notiService.deactiveAllByDocIdAndDocType(d.getId(), DocumentTypeEnum.VAN_BAN_DEN);

        // Inactive process
        processService.updateByActive(d.getId(), false);

        // Notification
        notiService.addAll(userIds, parentId, d.getPreview(), type, NotificationHandleStatusEnum.TU_CHOI_TIEP_NHAN,
                module);

        // Update status doc
        d.setStatus(DocumentStatusEnum.REJECT_RECEIVE);
        docRepository.save(d);

        // TODO : Add comment and file
        return true;
    }

    public Boolean rejectReceiveDocByOutsideId(Documents d, String comment, MultipartFile[] files) {
        // Inactive process
        processService.updateByActive(d.getId(), false);

        // Update status doc
        d.setStatus(DocumentStatusEnum.REJECT_RECEIVE);
        docRepository.save(d);

        // TODO : Add comment and file
        return true;
    }

    private List<Attachment> getAttachmentsByParent(Documents parent, List<Attachment> atms) {
        if (parent != null && !BussinessCommon.isEmptyList(parent.getAttachments())) {
            atms.addAll(parent.getAttachments());
            if (parent.getParent() != null && !BussinessCommon.isEmptyList(parent.getParent().getAttachments())) {
                getAttachmentsByParent(parent.getParent(), atms);
            }
        }
        return atms;
    }

    public List<Attachment> getListAttachmenByParentId(Documents documentsParent) {
        List<Attachment> attachmentList = new ArrayList<>();
        if (documentsParent != null) {
            List<Long> docIds = new ArrayList<>();
            docIds.add(documentsParent.getId());
            if (documentsParent.getParentId() != null) {
                docIds.add(documentsParent.getParentId());
                attachmentList = attachmentRepository.findByListDocId(docIds, BussinessCommon.getClientId());
            } else {
                attachmentList = documentsParent.getAttachments();
            }
        }
        return attachmentList;
    }

    public List<FindDocDto> exportExcels(Boolean expired, String numberOrSign, String preview,
                                         Long bookId, String orgIssuedName, Long docFieldsId, DocumentStatusEnum docStatusId,
                                         Long urgentId, Long securityId, Long numberArrival, Date startArrival,
                                         Date endArrival, Date startIssued, Date endIssued, Date startReceived, Date endReceived, Long orgReceiveId) {
        User u = BussinessCommon.getUser();
        Date now = DateTimeUtils.handleSubmit(new Date());
        List<Long> orgIds = orgService.orgAndSub(u.getOrg());
        List<Long> listUser = clericalOrgRepo.getClericalOrgByOrgIdAndClientId(orgService.getRootOrgId(u.getOrg()), BussinessCommon.getClientId());
        for (Long userIdVTDV : listUser) {
            User userOrg = userService.findByUserId(userIdVTDV);
            if (userOrg != null) {
                orgIds.add(userOrg.getOrg());
            }
        }
        List<Documents> documentsMap = docRepository.findAllIdDoc(expired, now, numberOrSign, bookId, preview,
                docFieldsId, docStatusId, urgentId, securityId, u.getClientId(), numberArrival, startArrival, endArrival,
                startIssued, endIssued, startReceived, endReceived, orgIssuedName, orgIds, Constant.VAN_THU_MAIN, orgService.getRootOrgId(u.getOrg()), orgReceiveId);
        int no = 0;
        List<FindDocDto> rsList = new ArrayList<>();
        for (Documents documents : documentsMap) {
            FindDocDto rs = new FindDocDto(documents);
            rs.setNo(++no);
            rsList.add(rs);
        }
        HashMap<Long, Long> hashMap = new HashMap<>();
        List<Long> listLong = new ArrayList<>();
        for (int i = 0; i < rsList.size(); i++) {
            if (rsList.get(i).getParentId() != null) {
                if (!hashMap.containsKey(rsList.get(i).getParentId())) {
                    hashMap.put(rsList.get(i).getParentId(), rsList.get(i).getParentId());
                    listLong.add(rsList.get(i).getId());
                }
            } else {
                listLong.add(rsList.get(i).getId());
            }
        }
        List<Long> orgIdBan = orgService.orgAndSub(orgService.getRootOrgId(BussinessCommon.getOrgId()));
        List<Long> userBanThuBan = userRepository.getUsersByNameCategory(Constant.VAN_THU_MAIN, orgIdBan);
        List<Documents> docList = docRepository.findAllDocNoPage(listLong, userBanThuBan);
        int no1 = 0;
        List<FindDocDto> objectArrayList = new ArrayList<>();
        hashMap = new HashMap<>();
        for (Documents documents : docList) {
            FindDocDto rs = new FindDocDto(documents);
            rs.setNo(++no1);
            objectArrayList.add(rs);
        }

        return objectArrayList;
    }


    public ObjectNode exportExcelByConfigs(String numberOrSignL, String previewL,
                                           Long bookIdL, String orgIssuedNameL, Long docFieldsIdL, DocumentStatusEnum docStatusIdL,
                                           Long urgentIdL, Long securityIdL, String numberArrivalL, Date startArrivalL,
                                           Date endArrivalL, Date startIssuedL, Date endIssuedL, Date startReceived, Date endReceived,
                                           HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus, Sort sort) {
        List<DocumentDto> t = exportExcelToLists(numberOrSignL, previewL, bookIdL,
                orgIssuedNameL, docFieldsIdL, docStatusIdL, urgentIdL, securityIdL, numberArrivalL,
                startArrivalL, endArrivalL, startIssuedL, endIssuedL, startReceived, endReceived,
                handleType, handleStatus, sort);
        return rpFieldService.getData(DocumentTypeEnum.VAN_BAN_DEN, t);
    }

    public List<DocumentDto> exportExcelToLists(String numberOrSignL, String previewL,
                                                Long bookIdL, String orgIssuedNameL, Long docFieldsIdL, DocumentStatusEnum docStatusIdL,
                                                Long urgentIdL, Long securityIdL, String numberArrivalL, Date startArrivalL,
                                                Date endArrivalL, Date startIssuedL, Date endIssuedL, Date startReceived, Date endReceived,
                                                HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus, Sort sort) {
        User u = BussinessCommon.getUser();
        boolean isLibrarian = userService.isVanThuVBDen(u);
        Date now = DateTimeUtils.handleSubmit(new Date());
        Boolean delegateDoc = null;
        if (DocumentStatusEnum.DELEGATE_DOC.equals(docStatusIdL)) {
            delegateDoc = true;
            docStatusIdL = null;
        }

        Integer endTask = null;
        if (DocumentInHandleStatusEnum.HOAN_THANH.equals(handleStatus)) { // Done tab
            endTask = 2;
            handleStatus = null;
        } else if (DocumentInHandleStatusEnum.DA_XU_LY.equals(handleStatus)) { // Did tab
            endTask = 0;
        }

        List<Long> witDocument = getWithDocument(BussinessCommon.getClientId(), clericalOrg, u.getId(), isLibrarian);
        List<Long> withProcess = getWithProcesss(delegateDoc, handleType, handleStatus, endTask);
        return docRepository.findAdvanceAllNotPagings(now, u.getId(), numberOrSignL,
                StringUtils.decodeFromUrl(previewL), bookIdL, StringUtils.decodeFromUrl(orgIssuedNameL),
                docFieldsIdL, docStatusIdL, urgentIdL, securityIdL, numberArrivalL, startArrivalL, endArrivalL,
                startIssuedL, endIssuedL, startReceived, endReceived,
                BussinessCommon.getClientId(), withProcess, witDocument, sort);
    }

    private List<Long> getWithProcesss(Boolean delegateDoc, HandleTypeEnum handleType,
                                       DocumentInHandleStatusEnum handleStatus, Integer endTask) {
        return docRepository.getWithProcesss(delegateDoc, handleType, handleStatus, endTask,
                BussinessCommon.getUserId(), BussinessCommon.getClientId());
    }

    public Documents update(Long id, Documents doc) {
        User u = BussinessCommon.getUser();
        if (!userService.isVanThuVBDen(u)) {
            throw new RestExceptionHandler(Message.NO_TRANSFER_HANDLE);
        }

        doc.valids();
        Documents old = valid(id, Message.NOT_FOUND_DOC);
        old.set(doc);

        try {
            save(doc);
        } catch (Exception e) {
            throw new RestExceptionHandler(Message.ERROR_SYS);
        }

        // update document_book
        DocumentBook db = dbService.findByBookId(doc.getBookId());
        if (db != null && doc.getNumberArrival() > db.getCurrentNumber()) {
            db.setCurrentNumber(doc.getNumberArrival());
            dbService.save(db);
        }
        return doc;
    }

    public List<Documents> findByDocOutId(Long id) {
        return docRepository.findByDocOutId(id, BussinessCommon.getClientId());
    }

    private boolean isNumberOrSignExists(String numberOrSign, final long clientId, List<Long> excludeDocIds) {
        return docRepository.isNumberOrSignExists(numberOrSign, clientId, excludeDocIds, true);
    }

    private void addPlaceSendOthers(Documents doc) {
        if (!StringUtils.isNullOrEmpty(doc.getPlaceSendOthers())) {
            Category placeSendOthers = categoryService.save(doc.getPlaceSendOthers(), Constant.CAT_ORG_OTHER, "Nơi nhận bên ngoài");
            doc.setPlaceSendId(placeSendOthers.getId());
            doc.setPlaceSend(doc.getPlaceSendOthers());
        }
    }
}
