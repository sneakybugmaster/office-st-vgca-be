package com.vz.backend.business.service;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.vz.backend.business.controller.DocumentOutController.Tab;
import com.vz.backend.business.domain.*;
import com.vz.backend.business.dto.*;
import com.vz.backend.business.dto.document.DocumentReceiveBasicDto;
import com.vz.backend.business.dto.document.IssuedDto;
import com.vz.backend.business.dto.fullreport.*;
import com.vz.backend.business.repository.*;
import com.vz.backend.business.service.hstl.HsFolderService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.*;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.CategoryType;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.*;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IAuthorityUserRepository;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.repository.IUserRepository;
import com.vz.backend.core.service.*;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;
import org.apache.commons.io.FilenameUtils;
import org.hibernate.Hibernate;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import java.util.Calendar;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class DocumentOutService extends BaseService<DocumentOut> {
    @Value("${configs.doc-out.auto-finish-reply-doc: false}")
    private boolean autoFinish;
    @Value("${configs.clerical-org: false}")
    private boolean clericalOrg;
    @Value("${configs.doc-out.print-issued-data: false}")
    private boolean printIssuedData;
    @Value("${configs.auto-number-in-book-doc-out: true}")
    private boolean isAutoNumberInBook;

    @Autowired
    IDocumentOutRepository docOutRepo;

    @Autowired
    IDocumentInProcessRepository dipRepo;

    @Autowired
    IDocumentOutProcessRepository dopRepo;

    @Autowired
    ITaskRepository taskRepo;

    @Autowired
    OrganizationService orgService;

    @Autowired
    DocumentOutProcessService docOutProcessService;

    @Autowired
    DocumentOutTrackingService docOutTrackingService;

    @Autowired
    DocumentOutAttachmentService docOutAttachService;

    @Autowired
    DocumentOutCommentService docOutCommentService;

    @Autowired
    DocumentReceiveService docReceiveService;

    @Autowired
    CategoryService catService;

    @Autowired
    UserService userService;

    @Autowired
    CategoryTypeService catTypeService;

    @Autowired
    DocumentService docService;

    @Autowired
    DocumentBookService dbookService;

    @Autowired
    DocumentReceiveService drService;

    @Autowired
    RoleService roleService;

    @Autowired
    DocumentBookService documentBookService;

    @Autowired
    DocumentInProcessService prService;

    @Autowired
    NotificationService notiService;

    @Autowired
    DocumentOutAttachmentService doaService;

    @Autowired
    private StraceSystemService straceService;

    @Autowired
    RoleService rService;

    @Autowired
    DelegateService delegateService;

    @Autowired
    IDocumentOutAttachmentRepository doaRepo;

    @Autowired
    AttachmentVersionService attactVersionService;

    @Autowired
    DocumentInProcessService docInProcessService;

    @Autowired
    TaskService taskService;

    @Autowired
    DocumentUserService docUserService;

    @Autowired
    DocumentBookService docBookService;

    @Autowired
    private DocumentInOutService docInOutService;

    @Autowired
    private DocumentOutTaskService docOutTaskService;

    @Autowired
    private BpmnService2 bpmnService;

    @Autowired
    private HsFolderService hsFolderService;

    @Autowired
    private Calendar2Service calendarService;

    @Autowired
    private ObjectReadService objReadService;

    @Autowired
    private ReportFieldService rpFieldService;

    @Autowired
    private IOutsideReceiveDocumentRepository outsideReceiveRepo;

    @Autowired
    private ClericalOrgService clericalOrgService;

    @Autowired
    private IDocumentReceiveRepository docReceiveRepo;

    @Autowired
    private IDocumentOutTrackingRepository docOutTrackingRepo;

    @Autowired
    private IAuthorityUserRepository authorityUserRepository;

    @Autowired
    private FilesStorageService fileStorageService;

    @Autowired
    private IUserRepository userRepository;

    @Autowired
    private IClericalOrgRepository clericalOrgRepository;

    @Autowired
    private DocumentService documentService;

    @Autowired
    private AttachmentService attachmentService;

    @Autowired
    private CommonService commonService;

    @Autowired
    private DocumentInTrackingService trackingService;

    @Autowired
    private DocumentInProcessService processService;
    private final Map<Long, Documents> orgsDocumentsMap = new HashMap<>();

    @Autowired
    private DocumentOutHistoryService documentOutHistoryService;

    @PersistenceContext
    private EntityManager entityManager;


    @Override
    public IRepository<DocumentOut> getRepository() {
        return docOutRepo;
    }

    public boolean checkStatusInList(Long docId, List<DocumentStatusEnum> listStatus) {
        return docOutRepo.checkStatusInList(docId, listStatus);
    }

    public DocumentOut findByDocId(long docId) {
        Optional<DocumentOut> oDoc = docOutRepo.findById(docId);
        if (!oDoc.isPresent()) {
            throw new RestExceptionHandler(Message.DOCUMENT_NOT_FOUND);
        }
        return oDoc.get();
    }

    @Transactional
    public DocumentOut findByDocIdHibernated(long docId) {
        DocumentOut documentOut = entityManager.find(DocumentOut.class, docId);
        if (documentOut != null) {
            Hibernate.initialize(documentOut.getListReceive());
            Hibernate.initialize(documentOut.getDocumentBook());
            Hibernate.initialize(documentOut.getDocType());
            Hibernate.initialize(documentOut.getUrgent());
            Hibernate.initialize(documentOut.getSecurity());
            Hibernate.initialize(documentOut.getDocField());
            Hibernate.initialize(documentOut.getOutsideReceives());
            return documentOut;
        } else {
            return null;
        }
    }

    @Override
    public DocumentOut save(DocumentOut input) {
        DocumentOut result;
        try {
            result = docOutRepo.save(input);
        } catch (Exception e) {
            throw new RestExceptionHandler(Message.NUMBER_IN_BOOK_EXIST);
        }
        return result;
    }

    public void validateDocumentOutInput(DocumentOut input) {
        if (input == null || input.getDocTypeId() == null || StringUtils.isNullOrEmpty(input.getPreview())) {
            throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
        }
        if (input.getDocFieldId() == null || input.getDocFieldId() == 0) {
            input.setDocFieldId(null);
        }

        BussinessCommon.validLengthData(input.getNumberOrSign(), "Số/Ký hiệu", 100);
        BussinessCommon.validLengthData(input.getListSignersName(), "Người kí", 250);

        for (OutsideReceiveDocument i : input.getOutsideReceives()) {
            BussinessCommon.require("Nơi nhận bên ngoài", i.getAddress());
            BussinessCommon.validLengthData(i.getAddress(), "Nơi nhận bên ngoài", 250);
        }
    }

    private String getOrgCreateName(Long orgCreateId) {
        String tmp = BussinessCommon.getUser().getOrgModel().getName();
        if (orgCreateId == null) {
            return tmp;
        }

        return orgService.valid(orgCreateId, Message.NOT_FOUND_ORG).getName();
    }

    public void validateDaBanHanhDocumentOutInput(DocumentOut input) {
        BussinessCommon.require("Người ký", input.getListSignersName());
    }

    @Transactional
    public DocumentOut create(DocumentOut input) {
        // validate doc input
        validateDocumentOutInput(input);
        input.setIsInternalDocument(BussinessCommon.getValueOrDefault(input.getIsInternalDocument(), false));

        // validate Người ký của Dự thảo văn bản ban hành (ticket 2224)
        if (Boolean.TRUE.equals(input.getIssued())) {
            validateDaBanHanhDocumentOutInput(input);
        }

        // save doc
        input.setOrgCreateName(getOrgCreateName(input.getOrgCreateId())); // setOrgCreatName
        input.setOrgIssuedId(BussinessCommon.getUser().getOrg());
        // Vào sổ văn bản
        if (input.getBookId() != null) {
            documentBookService.updateCurrentNumber(input.getBookId(), input.getNumberInBook());
        }
        //set NumberInBook
        if (!isAutoNumberInBook) {
            if (input.getNumberOrSign() != null) {
                String[] numberInBook = input.getNumberOrSign().split("/", 10);
                try {
                    if (numberInBook[0].split("-").length > 0) {
                        input.setNumberInBook(Long.parseLong(numberInBook[0].split("-")[0]));
                    } else {
                        input.setNumberInBook(Long.parseLong(numberInBook[0]));
                    }
                } catch (Exception e) {
                    throw new RestExceptionHandler(Message.ERROR_NUMBER_IN_BOOK);
                }
            }
        }
        if (input.getHasIssued() != null && !input.getHasIssued()) {
            input.setStatus(DocumentStatusEnum.CHO_BAN_HANH);
        }
        input.setDocBaseModel();
        input = save(input);

        DocumentBook documentBook = null;
        if (input.getBookId() != null) {
            documentBook = documentBookService.findByBookId(input.getBookId());
            input.setBookName(documentBook.getName());
        }


        if (input.getDocTypeId() != null) {
            Optional<Category> docTypeCateOptional = catService.findById(input.getDocTypeId());
            if (docTypeCateOptional.isPresent()) {
                input.setDocTypeName(docTypeCateOptional.get().getName());
            }
        }

        if (input.getSecurityId() != null) {
            Optional<Category> docSecurityOptional = catService.findById(input.getSecurityId());
            if (docSecurityOptional.isPresent()) {
                input.setDocSecurityName(docSecurityOptional.get().getName());
            }
        }

        if (input.getUrgentId() != null) {
            Optional<Category> docUrgentOptional = catService.findById(input.getSecurityId());
            if (docUrgentOptional.isPresent()) {
                input.setDocUrgentName(docUrgentOptional.get().getName());
            }
        }


        documentOutHistoryService.save(input, DocumentOutActionEnum.CREATE);
        // Add van ban den phuc dap
        Long[] docInIds = BussinessCommon.castStringToLongArray(input.getReplyDocIds());
        Set<Long> setDocInIds = new HashSet<>(Arrays.asList(docInIds));
        docInOutService.add(input.getId(), setDocInIds);
        // Add công việc
        Long[] taskIds = BussinessCommon.castStringToLongArray(input.getRelateTaskIds());
        Set<Long> setTaskIds = new HashSet<>(Arrays.asList(taskIds));
        docOutTaskService.add(input.getId(), setTaskIds);

        Boolean isInternalDocument = input.getIsInternalDocument();

        // add list receive person

        List<DocumentReceive> receivers = input.getListReceive().stream().peek(r -> r.setHandleType(isInternalDocument ? HandleTypeEnum.INTERNAL_INCOMING : HandleTypeEnum.INTERNAL_ISSUED_INCOMING)).collect(Collectors.toList());


        input.setListReceive(receivers);

        List<DocumentReceive> t = input.getListReceive();
        Long docId = input.getId();
        t = validReceiveId(t, docId);
        if (t != null && !t.isEmpty()) {
            docReceiveService.saveAll(t);
        }

        // add process
        if (input.getStatus() != DocumentStatusEnum.CHO_BAN_HANH) {
            docOutProcessService.save(input.getId(), BussinessCommon.getUser().getId(), BussinessCommon.getUser().getId(),
                    null, input.getHasIssued() != null && !input.getHasIssued() ? DocumentOutHandleStatusEnum.CHO_XU_LY : DocumentOutHandleStatusEnum.DU_THAO);
        }
        // add tracking
        docOutTrackingService.save(input.getId(), DocumentOutTrackingEnum.CREATE);

        // save strace
        straceService.save(input.getId(), ActionEnum.ADD.getName(), input.getPreview(), input.getIdCat(), null);

        //add outside receive document
        outsideReceiveRepo.saveAll(setDocInOutsideReceives(input.getOutsideReceives(), docId));
        return input;
    }

    private List<DocumentReceive> validReceiveId(List<DocumentReceive> drList, Long docId) {
        if (drList == null || drList.isEmpty()) {
            return null;
        }

        // valid type
        List<String> typeList = drList.stream().map(DocumentReceive::getType).collect(Collectors.toList());
        if (typeList == null || !typeList.contains(Constant.RECEIVER_TYPE_ORG)
                && !typeList.contains(Constant.RECEIVER_TYPE_USER) && !typeList.contains(Constant.RECEIVER_TYPE_ALL)) {
            throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT);
        }

        List<Long> orgIdList = orgService.getIdList(true);
        List<Long> userIdList = userService.getIdList(true);

        // valid receive id
        drList.stream().forEach(i -> {
            if (Constant.RECEIVER_TYPE_ORG.equals(i.getType()) || Constant.RECEIVER_TYPE_ALL.equals(i.getType())) {
                if (orgIdList != null && !orgIdList.contains(i.getReceiveId())) {
                    throw new RestExceptionHandler(Message.NOT_FOUND_ORG);
                }
            } else if (Constant.RECEIVER_TYPE_USER.equals(i.getType()) && (userIdList != null && !userIdList.contains(i.getReceiveId()))) {
                throw new RestExceptionHandler(Message.NOT_FOUND_USER);
            }
            i.setDocId(docId);
        });
        return drList;
    }

    private List<DocumentReceive> setDrList(List<DocumentReceive> drNewList, Long docId, int type) {
        if (drNewList == null || drNewList.isEmpty()) {
            return null;
        }
        List<DocumentReceive> oldDrList = drService.findByClientIdAndDocIdAndType(BussinessCommon.getClientId(), docId,
                null);
        List<Long> allTypeList = oldDrList.stream().filter(j -> Constant.RECEIVER_TYPE_ALL.equals(j.getType()))
                .map(DocumentReceive::getReceiveId).collect(Collectors.toList());
        List<Long> orgTypeList = oldDrList.stream().filter(j -> Constant.RECEIVER_TYPE_ORG.equals(j.getType()))
                .map(DocumentReceive::getReceiveId).collect(Collectors.toList());
        List<Long> userTypeList = oldDrList.stream().filter(j -> Constant.RECEIVER_TYPE_USER.equals(j.getType()))
                .map(DocumentReceive::getReceiveId).collect(Collectors.toList());
        List<Long> orgTypeList2 = drNewList.stream().filter(j -> Constant.RECEIVER_TYPE_ORG.equals(j.getType()))
                .map(DocumentReceive::getReceiveId).collect(Collectors.toList());
        List<Long> userTypeList2 = drNewList.stream().filter(j -> Constant.RECEIVER_TYPE_USER.equals(j.getType()))
                .map(DocumentReceive::getReceiveId).collect(Collectors.toList());
        List<Long> allTypeList2 = drNewList.stream().filter(j -> Constant.RECEIVER_TYPE_ALL.equals(j.getType()))
                .map(DocumentReceive::getReceiveId).collect(Collectors.toList());

        List<DocumentReceive> drDelList = new ArrayList<>();
        List<DocumentReceive> toRemove = new ArrayList<>();
        if (type == 1) {
            drNewList.stream().forEach(i -> {
                if (Constant.RECEIVER_TYPE_ORG.equals(i.getType()) && orgTypeList != null
                        && orgTypeList.contains(i.getReceiveId())
                        || Constant.RECEIVER_TYPE_USER.equals(i.getType()) && userTypeList != null
                        && userTypeList.contains(i.getReceiveId())
                        || Constant.RECEIVER_TYPE_ALL.equals(i.getType()) && allTypeList != null
                        && allTypeList.contains(i.getReceiveId())
                ) {
                    toRemove.add(i);
                }
            });
            drNewList.removeAll(toRemove);
        } else if (type == 2) {
            oldDrList.stream().forEach(j -> {
                if (Constant.RECEIVER_TYPE_ORG.equals(j.getType()) && orgTypeList2 != null
                        && !orgTypeList2.contains(j.getReceiveId())
                        || Constant.RECEIVER_TYPE_USER.equals(j.getType()) && userTypeList2 != null
                        && !userTypeList2.contains(j.getReceiveId())
                        || Constant.RECEIVER_TYPE_ALL.equals(j.getType()) && allTypeList2 != null
                        && !allTypeList2.contains(j.getReceiveId())
                ) {
                    drDelList.add(j);
                }
            });
        }

        if (type == 1) {
            return drNewList;
        }
        if (type == 2) {
            return drDelList;
        }
        return null;
    }

    public DocumentOut validateUpdate(Long docId) {
        DocumentOut data = findByDocId(docId);
        validateUpdate(data);
        return data;
    }

    public void validateUpdate(DocumentOut data) {
        // Văn bản dự thảo, bị trả lại, thu hồi xử lý thì người tạo được phép update
        if (DocumentStatusEnum.DU_THAO.equals(data.getStatus())
                && DocumentStatusEnum.BI_TRA_LAI.equals(data.getStatus())
                && DocumentStatusEnum.THU_HOI_XL.equals(data.getStatus())) {
            if (!data.getPersonEnterId().equals(BussinessCommon.getUserId())) {
                throw new RestExceptionHandler(Message.UPDATE_NOT_ALLOWED);
            }
            return;
        }
        // Văn bản chờ ban hành cho phép văn thư update lúc ban hành.
        if (DocumentStatusEnum.CHO_BAN_HANH.equals(data.getStatus())) {
            // Check văn thư
            if (!userService.isVanThuVBDiByOrg(BussinessCommon.getUser(), data.getOrgIssuedId())) {
                throw new RestExceptionHandler(Message.UPDATE_NOT_ALLOWED);
            }
            return;
        }
        // Văn bản đang xử lý, đã ban hành, thu hồi ban hành không được phép update
        // Check văn thư
        if ((DocumentStatusEnum.DANG_XU_LY.equals(data.getStatus())
                && DocumentStatusEnum.DA_BAN_HANH.equals(data.getStatus())
                && DocumentStatusEnum.THU_HOI_BH.equals(data.getStatus())) && !userService.isVanThuVBDiByOrg(BussinessCommon.getUser(), data.getOrgIssuedId())) {
            throw new RestExceptionHandler(Message.UPDATE_NOT_ALLOWED);
        }
    }

    @Transactional
    public boolean update(Long docId, DocumentOut input) {
        // validate doc input
        validateDocumentOutInput(input);
        // save doc
        DocumentOut data = findByDocIdHibernated(docId);
        validateUpdate(data);


        // Vào sổ văn bản
        Long numberInBook = input.getNumberInBook();
        if (numberInBook != null && !numberInBook.equals(data.getNumberInBook())) {
            docBookService.updateCurrentNumber(input.getBookId(), numberInBook);
        }

        input.setId(data.getId());
        input.setActive(data.getActive());
        input.setCreateBy(data.getCreateBy());
        input.setCreateDate(data.getCreateDate());
        input.setPersonEnterId(data.getPersonEnterId());
        input.setOrgIssuedId(data.getOrgIssuedId());

        /**
         * Tổ chức soạn thảo
         */
        if (input.getOrgCreateId() != null && !input.getOrgCreateId().equals(data.getOrgCreateId())) {
            input.setOrgCreateName(getOrgCreateName(input.getOrgCreateId())); // setOrgCreatName
        }
        documentOutHistoryService.save(input, DocumentOutActionEnum.UPDATE);
        //add outside receive document
        input.setOutsideReceives(setDocInOutsideReceives(input.getOutsideReceives(), docId));

        // Get for singer id
        List<Long> signerIds = new ArrayList<>();
        if (DocumentStatusEnum.DA_BAN_HANH.equals(data.getStatus())) {
            signerIds = getUserIdSignerByDocId(input.getListSignersName(), input.getListSignerIds(),
                    data.getListSignersName(), data.getListSignerIds());
        }

        data = save(input);


        // Update van ban den phuc dap
        Long[] docInIds = BussinessCommon.castStringToLongArray(input.getReplyDocIds());
        Set<Long> setIds = new HashSet<>(Arrays.asList(docInIds));
        docInOutService.add(data.getId(), setIds);
        // Update công việc
        Long[] taskIds = BussinessCommon.castStringToLongArray(input.getRelateTaskIds());
        Set<Long> setTaskIds = new HashSet<>(Arrays.asList(taskIds));
        docOutTaskService.add(input.getId(), setTaskIds);
        // update receive
        List<DocumentReceive> documentReceives;
        if (data.getIsInternalDocument() != null && data.getIsInternalDocument()) {
            documentReceives = input.getListReceive().stream().peek(dr -> dr.setHandleType(HandleTypeEnum.INTERNAL_INCOMING)).collect(Collectors.toList());
        } else {
            documentReceives = input.getListReceive().stream().peek(dr -> dr.setHandleType(HandleTypeEnum.INTERNAL_ISSUED_INCOMING)).collect(Collectors.toList());
        }
        input.setListReceive(documentReceives);

        List<DocumentReceive> addList = updateDrListByIdDoc(input.getListReceive(), docId, true);

        // add tracking (update lúc văn thư ban hành không log tracking).
        if (!DocumentStatusEnum.CHO_BAN_HANH.equals(data.getStatus())) {
            docOutTrackingService.save(data.getId(), DocumentOutTrackingEnum.UPDATE);
        }

        // Khi văn bản đã ban hành cho phép thêm nơi nhận nội bộ -> add tracking
        if (DocumentStatusEnum.DA_BAN_HANH.equals(data.getStatus())) {

            // Get user list by document receive
            List<Long> userIds = getUserIdReceiveByDocId(addList, false);

            // Add notification
            notiService.addAll(userIds, input.getId(), data.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                    NotificationHandleStatusEnum.DA_BAN_HANH, ModuleCodeEnum.DOCUMENT_IN_LIST);

            // Save tracking
            docOutTrackingService.saveAll(input.getId(), BussinessCommon.getUserId(), userIds,
                    DocumentOutTrackingEnum.INCOMING);

            // Add notification for signer Id
            notiService.addAll(signerIds, input.getId(), "Yêu cầu kí : " + data.getPreview(),
                    DocumentTypeEnum.VAN_BAN_DI, NotificationHandleStatusEnum.DA_BAN_HANH,
                    ModuleCodeEnum.DOCUMENT_IN_LIST);
        }

        data.setListReceive(input.getListReceive());
        if (input.getIssued() != null && input.getIssued()) {
            return autoIssued(data, null, true);
        }


        return true;
    }

    public List<OutsideReceiveDocument> setDocInOutsideReceives(List<OutsideReceiveDocument> outsideReceives,
                                                                Long docId) {
        List<OutsideReceiveDocument> rs = new ArrayList<>();
        for (OutsideReceiveDocument i : outsideReceives) {
            if (i.getId() == null) {
                i.setDocId(docId);
            } else {
                i = outsideReceiveRepo.findByClientIdAndId(BussinessCommon.getClientId(), i.getId());
                if (i == null) {
                    throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
                }
            }
            rs.add(i);
        }
        return rs;
    }

    public boolean updateListSigners(Long id, String listSigners) {
        DocumentOut doc = findByDocId(id);
        if (BussinessCommon.getUserId().equals(doc.getPersonEnterId())) {
            doc.setListSignerIds(listSigners);
            docOutRepo.save(doc);
            return true;
        }
        throw new RestExceptionHandler(Message.ACTION_FAILED);
    }

    /**
     * Update document receives
     *
     * @param t
     * @param id
     * @param isUpdate
     * @return addList
     */
    private List<DocumentReceive> updateDrListByIdDoc(List<DocumentReceive> t, Long id, Boolean isUpdate) {
        t = validReceiveId(t, id);
        List<DocumentReceive> delList = setDrList(t, id, 2);
        List<DocumentReceive> addList = setDrList(t, id, 1);

        // element is added
        if (addList != null && !addList.isEmpty()) {
            docReceiveService.saveAll(addList);
        }

        // element is removed
        if (Boolean.TRUE.equals(isUpdate) && (delList != null && !delList.isEmpty())) {
            List<Long> idDelList = delList.stream().map(DocumentReceive::getId).collect(Collectors.toList());
            docReceiveService.removeListRd(id, idDelList);
        }
        return addList;
    }

    @Transactional
    public Boolean delete(Long id) {
        Optional<DocumentOut> dc = docOutRepo.findById(id);
        if (dc.isPresent()) {
            DocumentOut doc = dc.get();
            boolean isSuperAdmin = roleService.checkCanRetake(doc.getOrgIssuedId(), DocumentTypeEnum.VAN_BAN_DI);
            boolean isCreator = doc.getPersonEnterId().equals(BussinessCommon.getUserId());

            if (!isCreator && !isSuperAdmin) {
                throw new RestExceptionHandler(Message.DELETE_NOT_ALLOWED);
            }

            //for case: current user is both super admin and creator
            if (isCreator && !isSuperAdmin && !DocumentStatusEnum.DU_THAO.equals(doc.getStatus())
                    && !DocumentStatusEnum.BI_TRA_LAI.equals(doc.getStatus()) && !DocumentStatusEnum.THU_HOI_XL.equals(doc.getStatus())) {
                throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
            }
            // Delete notification
            notiService.deactiveAllByDocIdAndDocType(id, DocumentTypeEnum.VAN_BAN_DI);
            // Delete document
            doc.setActive(false);
            docOutRepo.save(doc);
            doaService.deleteAllByDocId(doc.getId());
            return true;
        }
        throw new RestExceptionHandler(Message.ACTION_FAILED);
    }

    void updateNodeId(DocumentOut doc, Long nodeId) {
        doc.setNodeId(nodeId);
        docOutRepo.save(doc);
    }

    void updateNodeIdAndStatus(DocumentOut doc, Long nodeId, DocumentStatusEnum status) {
        doc.setStatus(status);
        updateNodeId(doc, nodeId);
    }

    void updateNodeIdAndStatusAndOrgIssued(DocumentOut doc, Long nodeId, DocumentStatusEnum status, Long orgId) {
        doc.setOrgIssuedId(orgId);
        updateNodeIdAndStatus(doc, nodeId, status);
    }

    public boolean autoIssued(DocumentOut doc, Long nodeId, Boolean createIncomingDocument) {
        try {
            DocumentBook db = documentBookService.findByBookId(doc.getBookId());
            long number = 1;
            if (db.getCurrentNumber() != null) {
                number = db.getCurrentNumber() + 1;
            }
            if (doc.getNumberInBook() == null) {
                doc.setNumberInBook(number);
                if (doc.getNumberOrSign() != null && doc.getNumberOrSign().trim().length() > 0) {
                    doc.setNumberOrSign(number + Constant.LINK_SIGN + doc.getNumberOrSign());
                } else {
                    String signLinkNumber = Constant.LINK_SIGN + db.getNumberOrSign();
                    doc.setNumberOrSign(number + (db.getNumberOrSign() != null ? signLinkNumber : ""));
                }
                db.setCurrentNumber(number);
                documentBookService.save(db);
            }
            doc.setNodeId(nodeId);
            doc.setStatus(DocumentStatusEnum.DA_BAN_HANH);
            doc.setDateIssued(new Date());
            doc.setOrgIssuedId(BussinessCommon.getUser().getOrg());
            save(doc);
            // Cần lấy ra danh sách người nhận để biết để add notification cho nó.
            List<Long> listUserIds = getReceiversUsersId(doc);

            List<Long> orgCreateLeaders = orgService.getOrgLeaders(doc.getOrgCreateId()).stream().map(User::getId).collect(Collectors.toList());
            if (doc.getIsInternalDocument() && orgCreateLeaders.size() > 0) {
                createIncomingDocumentForOrg(doc, orgCreateLeaders);
            }

            DocumentTypeEnum documentTypeEnum = null;
            ModuleCodeEnum moduleCodeEnum = null;

            if (doc.getIsInternalDocument() != null && doc.getIsInternalDocument()) {
                documentTypeEnum = DocumentTypeEnum.VAN_BAN_NOI_BO;
                moduleCodeEnum = ModuleCodeEnum.DOC_INTERNAL_INCOMING;
            } else {
                documentTypeEnum = DocumentTypeEnum.VAN_BAN_DEN_NOI_BO;
                moduleCodeEnum = ModuleCodeEnum.DOC_INTERNAL_ISSUED_INCOMING;
            }

            // Add notification
            notiService.addAll(listUserIds, doc.getId(), doc.getPreview(), documentTypeEnum,
                    NotificationHandleStatusEnum.DA_BAN_HANH, moduleCodeEnum);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return true;
    }

    private List<Long> getReceiversUsersId(DocumentOut doc) {
        List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdAndType(BussinessCommon.getClientId(),
                doc.getId(), null);
        List<Long> listOrgIds = new ArrayList<>();
        List<Long> listUserIds = new ArrayList<>();
        for (DocumentReceive dr : receiveIdList) {
            if (Constant.RECEIVER_TYPE_USER.equals(dr.getType())) {
                listUserIds.add(dr.getReceiveId());
            } else {
                listOrgIds.add(dr.getReceiveId());
            }
        }
        List<Long> listUserIdByOrgs = userService.getListLeadUserIdByOrg(listOrgIds);
        if (listUserIdByOrgs != null) {
            listUserIds.addAll(listUserIdByOrgs);
        }
        return listUserIds.stream().distinct().collect(Collectors.toList());
    }

    private void createIncomingDocumentForOrg(DocumentOut doc, List<Long> personalReceiversIds) {

        Map<Long, List<Long>> personalReceiversIdsByOrgIdMap = constructMapOfUsersIdByOrgIdFromListUsersId(personalReceiversIds);

        sendIncomingDocumentToUsers(doc, personalReceiversIdsByOrgIdMap);
    }

    private Map<Long, List<Long>> constructMapOfUsersIdByOrgIdFromListUsersId(List<Long> usersIdsList) {
        Map<Long, List<Long>> usersIdsByOrgIdMap = new HashMap<>();
        usersIdsList.forEach(id -> {
            User user = userService.findByUserId(id);
            Long orgId = user.getOrg();
            if (usersIdsByOrgIdMap.containsKey(orgId)) {
                usersIdsByOrgIdMap.get(orgId).add(id);
            } else {
                List<Long> usersIds = new ArrayList<>();
                usersIds.add(id);
                usersIdsByOrgIdMap.put(orgId, usersIds);
            }
        });
        return usersIdsByOrgIdMap;
    }

    private void sendIncomingDocumentToUsers(DocumentOut doc, Map<Long, List<Long>> orgUsersIdMap) {
        HandleTypeEnum handleType = doc.getIsInternalDocument() != null && doc.getIsInternalDocument() ? HandleTypeEnum.INTERNAL_INCOMING : HandleTypeEnum.INTERNAL_ISSUED_INCOMING;
        orgUsersIdMap.forEach((orgId, userIdsList) -> {
            Documents orgDocument;
            if (!orgsDocumentsMap.containsKey(orgId)) {
                Documents newDocument = createIncomingDocumentForOrg(doc, orgId);
                orgsDocumentsMap.put(orgId, newDocument);
            }
            orgDocument = orgsDocumentsMap.get(orgId);

            for (Long userId : userIdsList) {
                User user = userService.findByUserId(userId);
                saveDocumentProcess(doc, orgDocument, handleType, userId, user);
            }
        });
        orgsDocumentsMap.clear();
    }

    private void saveDocumentProcess(DocumentOut doc, Documents incomingDoc, HandleTypeEnum handleType, Long userId, User user) {
        trackingService.save(incomingDoc, DocumentInTrackingEnum.RECEIVE, userId, BussinessCommon.getClientId());

        // save process
        DocumentInProcess f = processService.findByDocIdAndFirstStep(doc.getId());
        if (f == null) {
            processService.save(Constant.START_NODE, incomingDoc.getId(), DocumentInHandleStatusEnum.CHO_XU_LY,
                    handleType, Constant.START_STEP, Constant.START_STEP, user, null, null,
                    incomingDoc.getDeadline(), null);
        } else {
            f.setToUser(userId);
            f.setHandleStatus(DocumentInHandleStatusEnum.CHO_XU_LY);
            processService.save(f);
        }

        commonService.createDocumentUserAndObjectRead(user, incomingDoc, BussinessCommon.getClientId());
    }

    private Documents createIncomingDocumentForOrg(DocumentOut doc, Long orgReceiveId) {
        Documents document = setDocumentsFields(doc, orgReceiveId);

        document.setStatus(DocumentStatusEnum.DOING);

        document = documentService.save(document);

        return document;
    }

    private void addAttachments(DocumentOut doc, Documents document) {
        List<Attachment> attachments = new ArrayList<>();
        for (DocumentOutAttachment a : doc.getAttachments()) {
            Attachment attachment = new Attachment();
            attachment.setName(a.getName());
            attachment.setType(a.getType());
            attachment.setSize(a.getSize());
            attachments.add(attachment);
        }
        attachmentService.addListAttachment(attachments, Collections.singletonList(document));
    }

    private Documents setDocumentsFields(DocumentOut doc, Long orgReceiveId) {
        Documents document = new Documents();
        document.setIsImported(true);
        document.setIsInternalDocument(true);
        document.setDocOutId(doc.getId());
        document.setDateArrival(doc.getDateIssued());
        document.setReceivedDate(doc.getDateIssued());
        String numberAndSign = "";
        if (doc.getNumberInBook() != null) {
            numberAndSign += doc.getNumberInBook();
        }
        if (doc.getNumberOrSign() != null) {
            numberAndSign += "/" + doc.getNumberOrSign();
        }
        document.setNumberOrSign(numberAndSign);
        document.setSecurityId(doc.getSecurityId());
        document.setUrgentId(doc.getUrgentId());
        document.setDocTypeId(doc.getDocTypeId());
        document.setPreview(doc.getPreview());
        document.setCreateBy(BussinessCommon.getUserId());
        document.setClientId(BussinessCommon.getClientId());
        document.setPersonEnterId(BussinessCommon.getUserId());
        document.setOrgReceiveId(orgReceiveId);
        document.setPersonSign(doc.getListSignersName());
        document.setBookId(doc.getBookId());
        document.setNumberArrival(doc.getNumberInBook());
        Category issuedOrgCategory = getOrCreateCategoryByNameAndType(doc.getOrgCreateName(), Constant.CAT_ORG_OTHER);
        document.setPlaceSendId(issuedOrgCategory.getId());
        document.setPlaceSend(issuedOrgCategory.getName());
        document.setPersonSign(doc.getSignerName());

        document = documentService.save(document);

        addAttachments(doc, document);

        return document;
    }

    private Category getOrCreateCategoryByNameAndType(String name, String issuedOrgCategoryTypeCode) {
        Category issuedOrgCategory = catService.findByNameAndCategoryCode(name, issuedOrgCategoryTypeCode);

        if (issuedOrgCategory == null) {
            Category newIssuedOrgCategory = new Category();
            CategoryType categoryType = catTypeService.findByClientIdAndCode(BussinessCommon.getClientId(), issuedOrgCategoryTypeCode);
            if (categoryType == null) {
                throw new RestExceptionHandler(String.format("Cannot find a CategoryType with cod '%s'", issuedOrgCategoryTypeCode));
            }
            newIssuedOrgCategory.setCategoryTypeId(categoryType.getId());
            newIssuedOrgCategory.setName(name);
            newIssuedOrgCategory.setActive(true);
            newIssuedOrgCategory.setClientId(BussinessCommon.getClientId());
            newIssuedOrgCategory.setCreateDate(new Date());
            newIssuedOrgCategory.setCreateBy(BussinessCommon.getClientId());
            issuedOrgCategory = catService.save(newIssuedOrgCategory);
        }
        return issuedOrgCategory;
    }

    @Transactional
    public boolean issued(Long docId) {
        DocumentOut doc = findByDocId(docId);
        if ((!DocumentStatusEnum.DU_THAO.equals(doc.getStatus())
                && !DocumentStatusEnum.CHO_BAN_HANH.equals(doc.getStatus())) || !userService.isVanThuVBDiByOrg(BussinessCommon.getUser(), doc.getOrgIssuedId())) {
            throw new RestExceptionHandler(Message.ISSUED_NOT_ALLOWED);
        }
        try {
            doc.setStatus(DocumentStatusEnum.DA_BAN_HANH);
            if (doc.getDateIssued() == null) {
                doc.setDateIssued(new Date());
            }
            doc.setOrgIssuedId(BussinessCommon.getUser().getOrg());
            if (doc.shouldEncrypt()) {
                doaService.encrypt(doc);
            }
            docOutRepo.save(doc);
            // Xóa tất cả thông báo trước đó
            notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DI, false);

            // Cần lấy ra danh sách người nhận để biết để add notification cho nó.
//            List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdAndType(BussinessCommon.getClientId(),
//                    docId, null);

            List<DocumentReceive> receiveIdList = drService.findByDocIdAndTypeAndClientIdAndActive(docId, null, BussinessCommon.getClientId(), true);

            // Get user list by document receive
            List<Long> userIds = getUserIdReceiveByDocId(receiveIdList, false);

            DocumentTypeEnum documentTypeEnum = null;
            ModuleCodeEnum moduleCodeEnum = null;

            if (doc.getIsInternalDocument() != null && doc.getIsInternalDocument()) {
                documentTypeEnum = DocumentTypeEnum.VAN_BAN_NOI_BO;
                moduleCodeEnum = ModuleCodeEnum.DOC_INTERNAL_INCOMING;
            } else {
                documentTypeEnum = DocumentTypeEnum.VAN_BAN_DEN_NOI_BO;
                moduleCodeEnum = ModuleCodeEnum.DOC_INTERNAL_ISSUED_INCOMING;
            }

            // Save tracking
            docOutTrackingService.saveAll(docId, BussinessCommon.getUserId(), userIds, DocumentOutTrackingEnum.INCOMING);

            // Add notification for singer
            List<Long> signerIds = getUserIdSignerByDocId(doc.getListSignersName(), doc.getListSignerIds(), null, null);
            notiService.addAll(signerIds, doc.getId(), "Yêu cầu kí : " + doc.getPreview(),documentTypeEnum,
                    NotificationHandleStatusEnum.DA_BAN_HANH, moduleCodeEnum);
            if (printIssuedData) {
                beforePrintDocIssuedData(docId, doc.getNumberOrSign(), doc.getDateIssued());
            }

            List<Long> orgCreateLeaders = orgService.getOrgLeaders(doc.getOrgCreateId()).stream().map(User::getId).collect(Collectors.toList());
            if (doc.getIsInternalDocument() && orgCreateLeaders.size() > 0) {
                createIncomingDocumentForOrg(doc, orgCreateLeaders);
            }

            // Add notification
            notiService.addAll(userIds, docId, doc.getPreview(), documentTypeEnum,
                    NotificationHandleStatusEnum.DA_BAN_HANH, moduleCodeEnum);

        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
        return true;
    }

    public boolean issued(IssuedDto input) {
        DocumentOut doc = findByDocId(input.getId());
        if (!userService.isVanThuVBDenByOrg(BussinessCommon.getUser(), doc.getOrgIssuedId()) || !DocumentStatusEnum.CHO_BAN_HANH.equals(doc.getStatus())) {
            throw new RestExceptionHandler(Message.ISSUED_NOT_ALLOWED);
        }

        if (input.getBookId() == null) {
            throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
        }

        // Update sổ văn bản
        Long numberInBook = input.getNumberInBook();
        if (numberInBook != null && !numberInBook.equals(doc.getNumberInBook())) {
            docBookService.updateCurrentNumber(input.getBookId(), numberInBook);
        }

        String signerNames = input.getListSignersName();
        BussinessCommon.validLengthData(signerNames, "Người kí", 250);

        // update receive
        updateDrListByIdDoc(input.getListReceive(), input.getId(), true);
        doc.setBookId(input.getBookId());
        doc.setNumberInBook(input.getNumberInBook());
        doc.setNumberOrSign(input.getNumberOrSign());
        doc.setStatus(DocumentStatusEnum.DA_BAN_HANH);
        if (input.getDateIssued() != null) {
            doc.setDateIssued(input.getDateIssued());
        }
        doc.setOrgIssuedId(BussinessCommon.getUser().getOrg());
        if (doc.shouldEncrypt()) {
            doaService.encrypt(doc);
        }

        // Người kí
        if (!StringUtils.isNullOrEmpty(signerNames)) {
            doc.setListSignersName(signerNames);
        }

        doc = save(doc);

        // Cần lấy ra danh sách người nhận để biết để add notification cho nó.
        List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdAndType(BussinessCommon.getClientId(), input.getId(), null);

        // Get user list by document receive
        List<Long> userIds = getUserIdReceiveByDocId(receiveIdList, false);

        // Add notification
        notiService.addAll(userIds, input.getId(), doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                NotificationHandleStatusEnum.DA_BAN_HANH, ModuleCodeEnum.DOCUMENT_IN_LIST);

        // Save tracking
        docOutTrackingService.saveAll(input.getId(), BussinessCommon.getUserId(), userIds, DocumentOutTrackingEnum.INCOMING);

        //add document receive
        transferOutside(doc, receiveIdList);

        // Add notification for singer
        List<Long> signerIds = getUserIdSignerByDocId(doc.getListSignersName(), doc.getListSignerIds(), null, null);
        notiService.addAll(signerIds, doc.getId(), "Yêu cầu kí : " + doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                NotificationHandleStatusEnum.DA_BAN_HANH, ModuleCodeEnum.DOCUMENT_IN_LIST);
        if (printIssuedData) {
            beforePrintDocIssuedData(doc.getId(), input.getNumberOrSign(), input.getDateIssued());
        }
        return true;
    }

    /**
     * Get user Id by signer name
     *
     * @param signerNames
     * @return
     */
    private List<Long> getUserIdsBySignerNames(String signerNames) {
        if (StringUtils.isNullOrEmpty(signerNames))
            return new ArrayList<>();
        String[] tmp = signerNames.split(",");
        for (String i : tmp) {
            try {
                i.trim();
            } catch (Exception e) {
                e.printStackTrace();
            }
        }

        return userService.findUserIdByFullName(tmp);
    }

    /**
     * Get user Id by signer id
     *
     * @param signerIds
     * @return
     */
    private List<Long> getUserIdsBySignerIds(String signerIds) {
        if (StringUtils.isNullOrEmpty(signerIds))
            return new ArrayList<>();
        List<Long> rs = new ArrayList<>();
        String[] tmp = signerIds.split(",");
        for (String i : tmp) {
            try {
                rs.add(Long.valueOf(i.trim()));
            } catch (Exception e) {
                continue;
            }
        }
        return rs;
    }

    /**
     * Get userId signer by docId
     *
     * @param nSignerNames
     * @param nSignerIds
     * @param oSignerNames
     * @param oSignerIds
     * @return
     */
    private List<Long> getUserIdSignerByDocId(String nSignerNames, String nSignerIds, String oSignerNames,
                                              String oSignerIds) {
        List<Long> rs = new ArrayList<>();
        List<Long> all = new ArrayList<>();
        List<Long> news = new ArrayList<>();
        news.addAll(getUserIdsBySignerIds(nSignerIds));
        news.addAll(getUserIdsBySignerNames(nSignerNames));

        List<Long> old = new ArrayList<>();
        old.addAll(getUserIdsBySignerIds(oSignerIds));
        old.addAll(getUserIdsBySignerNames(oSignerNames));

        all.addAll(old);
        all.addAll(news);

        // get news data
        all.forEach(i -> {
            if (!old.contains(i) && news.contains(i)) {
                rs.add(i);
            }
        });

        return rs;
    }

    /**
     * Get user list by document receive
     *
     * @param receiveIdList
     * @return
     */
    public List<Long> getUserIdReceiveByDocId(List<DocumentReceive> receiveIdList, boolean isForward) {
        if (BussinessCommon.isEmptyList(receiveIdList))
            return new ArrayList<>();
        List<Long> orgIds = new ArrayList<>();
        List<Long> orgIdGetAlls = new ArrayList<>();
        List<Long> userIds = new ArrayList<>();
        for (DocumentReceive dr : receiveIdList) {
            if (dr.getType() == null)
                continue;
            switch (dr.getType()) {
                case Constant.RECEIVER_TYPE_USER:
                    userIds.add(dr.getReceiveId());
                    break;
                case Constant.RECEIVER_TYPE_ORG:
                    orgIds.add(dr.getReceiveId());
                    break;
                case Constant.RECEIVER_TYPE_ALL:
                    orgIdGetAlls.add(dr.getReceiveId());
                    break;
                case Constant.RECEIVER_TYPE_FORWARD:
                    if (isForward) {
                        userIds.add(dr.getReceiveId());
                    }
                    break;
                default:
                    break;
            }
        }
        if (!orgIds.isEmpty()) {
            List<Long> userIdsA = userService.getListLeadUserIdByOrg(orgIds);
            List<User> lstUsers = userService.findByIds(userIdsA, true);
            for (User user : lstUsers) {
                try {
                    if (user.getPositionModel().getIsLeadership() && user.getPositionModel().getIsSiblings()) {
                        userIds.add(user.getId());
                    }
                } catch (Exception e) {
                }

            }
            for (Long org : orgIds) userIds.addAll(userService.getListVanThuByOrgIdAndCodeInRemoveVT(org));
        }
        if (!orgIdGetAlls.isEmpty()) {
            userIds.addAll(userService.findUserIdByOrgIds(orgIdGetAlls));
        }
        return userIds;
    }

    public List<Long> getUserIdDocumentReceiveByDocId(List<DocumentReceive> receiveIdList, boolean isForward) {
        if (BussinessCommon.isEmptyList(receiveIdList))
            return new ArrayList<>();
        List<Long> orgIds = new ArrayList<>();
        List<Long> orgIdGetAlls = new ArrayList<>();
        List<Long> userIds = new ArrayList<>();
        for (DocumentReceive dr : receiveIdList) {
            if (dr.getType() == null)
                continue;
            switch (dr.getType()) {
                case Constant.RECEIVER_TYPE_USER:
                    userIds.add(dr.getReceiveId());
                    break;
                case Constant.RECEIVER_TYPE_ORG:
                    orgIds.add(dr.getReceiveId());
                    break;
                case Constant.RECEIVER_TYPE_ALL:
                    orgIdGetAlls.add(dr.getReceiveId());
                    break;
                case Constant.RECEIVER_TYPE_FORWARD:
                    if (isForward) {
                        userIds.add(dr.getReceiveId());
                    }
                    break;
                default:
                    break;
            }
        }

        if (!orgIds.isEmpty()) {
            for (Long org : orgIds) {
                userIds.addAll(userService.getListIdsVanThuVBDiByOrg(org));
            }
        }
        if (!orgIdGetAlls.isEmpty()) {
            userIds.addAll(userService.findUserIdByOrgIds(orgIdGetAlls));
        }

        return userIds;
    }

    @Transactional
    public boolean issued(List<Long> listDocIds) {
        List<DocumentOut> listDocs = docOutRepo.findAllById(listDocIds);
        if (listDocs == null || listDocs.isEmpty()) {
            throw new RestExceptionHandler(Message.DOCUMENT_NOT_FOUND);
        }
        List<DocumentOut> listDocs1 = new ArrayList<>();
        for (int i = 0; i < listDocs.size(); i++) {
            DocumentOut doc = listDocs.get(i);
            boolean isVanThu = userService.isVanThuVBDiByOrg(BussinessCommon.getUser(), doc.getOrgIssuedId());
            if (DocumentStatusEnum.CHO_BAN_HANH.equals(doc.getStatus()) && isVanThu && doc.getBookId() != null) {
                DocumentBook db = documentBookService.findByBookId(doc.getBookId());
                long number = db.getCurrentNumber() + 1;
                doc.setStatus(DocumentStatusEnum.DA_BAN_HANH);
                doc.setDateIssued(new Date());
                if (doc.shouldEncrypt()) {
                    doaService.encrypt(doc);
                }
                if (doc.getNumberInBook() == null) {
                    db.setCurrentNumber(number);
                    documentBookService.save(db);
                    doc.setNumberInBook(number);
                    if (doc.getNumberOrSign() != null && doc.getNumberOrSign().trim().length() > 0) {
                        doc.setNumberOrSign(number + Constant.LINK_SIGN + doc.getNumberOrSign());
                    } else {
                        doc.setNumberOrSign(number + Constant.LINK_SIGN + db.getNumberOrSign());
                    }
                }
                // Xóa tất cả thông báo trước đó
                notiService.setActiveByDocIdAndDocType(doc.getId(), DocumentTypeEnum.VAN_BAN_DI, false);
                // Cần lấy ra danh sách người nhận để biết để add notification cho nó.
                List<Long> listUserIds = getReceiversUsersId(doc);
                // Add notification
                notiService.addAll(listUserIds, doc.getId(), doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                        NotificationHandleStatusEnum.DA_BAN_HANH, ModuleCodeEnum.DOCUMENT_IN_LIST);
                listDocs1.add(doc);
                if (printIssuedData) {
                    beforePrintDocIssuedData(doc.getId(), doc.getNumberOrSign(), doc.getDateIssued());
                }
            }
        }
        docOutRepo.saveAll(listDocs1);
        return true;
    }

    public DocumentOutDetailDto getDetailToEdit(Long docId) {
        return getDetailById(docId);
    }

    @Transactional
    public DocumentOutDetailDto getDetailToShow(Long docId) {
        // Set read thông báo
        notiService.setReadByDocIdAndDocTypeAndUserId(true, docId, DocumentTypeEnum.VAN_BAN_DI,
                BussinessCommon.getUserId());

        docOutTrackingService.save(docId, DocumentOutTrackingEnum.READ);
        return getDetailById(docId);
    }

    public DocumentOutDetailDto getDetailById(Long docId) {
        DocumentOut doc = findByDocId(docId);
        DocumentOutDetailDto docOut = new DocumentOutDetailDto(doc);
        List<Documents> docIns = docService.findByDocOutId(doc.getId());
        if (!docIns.isEmpty()) {
            docOut.setIncomingDocumentId(docIns.get(0).getId());
        }
        docOut.setEditable(
                BussinessCommon.getUserId().equals(doc.getPersonEnterId())
                        && docOutProcessService.isNoProcess(docId)
        );
//		validatePermission(doc);
        if (doc.getCreateBy() != null) {
            docOut.setPersonEnterName(userService.getOne(doc.getCreateBy()).getFullName());
        }
        if (doc.getStatus() != null) {
            docOut.setDocStatusName(doc.getStatus().getName());
        }
        if (doc.getBookId() != null) {
            docOut.setBookName(documentBookService.getOne(doc.getBookId()).getName());
        }
        List<Long> catIds = new ArrayList<>(Arrays.asList(doc.getDocFieldId(), doc.getDocTypeId(), doc.getSecurityId(), doc.getUrgentId()));
        List<Category> listCats = catService.findAllById(catIds);
        for (Category c : listCats) {
            if (doc.getDocFieldId() != null && doc.getDocFieldId().equals(c.getId())) {
                docOut.setDocFieldsName(c.getName());
            }
            if (doc.getDocTypeId() != null && doc.getDocTypeId().equals(c.getId())) {
                docOut.setDocTypeName(c.getName());
            }
            if (doc.getSecurityId() != null && doc.getSecurityId().equals(c.getId())) {
                docOut.setDocSecurityName(c.getName());
            }
            if (doc.getUrgentId() != null && doc.getUrgentId().equals(c.getId())) {
                docOut.setDocUrgentName(c.getName());
            }
        }
        // set list receive
        List<DocumentReceive> listReceivers = drService.findDRByClientIdAndDocIdTypeNotTransfer(BussinessCommon.getClientId(), docId, null);

        List<DocumentReceive> receivers = listReceivers.stream().peek(dr -> {
            if (dr.getType().equals("ORG")) {
                Organization o = orgService.findById(dr.getReceiveId()).orElse(null);
                if (o != null && o.getActive()) {
                    dr.setOrgName(o.getName());
                    dr.setOrgId(o.getId());
                }
            } else if (dr.getType().equals("USER")) {
                User u = userService.findByUserId(dr.getReceiveId());
                if (u != null && u.getActive()) {
                    dr.setFullName(u.getFullName());
                    dr.setOrgId(u.getOrg());
                }
            }
        }).collect(Collectors.toList());
        docOut.setListReceive(receivers);
//        docOut.setListReceive(getDataDrList(receiveIdList, docId));
        // Set list signers
        Long[] listIds = BussinessCommon.castStringToLongArray(doc.getListSignerIds());
        if (listIds != null) {
            docOut.setListSigners(getListSignersByIds(listIds));
        }
        // set list doc reply
        List<DocumentReplyDto> listReply = new ArrayList<>();
        for (DocumentInOut d : doc.getListRelateDoc()) {
            if (listReply.stream().noneMatch(reply -> reply.getId().equals(d.getDocInId()))) {
                listReply.add(new DocumentReplyDto(d.getDocIn()));
            }
        }
        docOut.setListReplyDoc(listReply);
        // set list relate task
        docOut.setListReplyTask(taskService.getListTaskDtoByDocIdAndDocType(docId, false));
        // set list reply task
        List<TaskDto> listRelateTask = new ArrayList<>();
        for (DocumentOutTask d : doc.getListDocumentOutTask()) {
            listRelateTask.add(new TaskDto(d.getTask()));
        }
        docOut.setListRelateTask(listRelateTask);
        // Set list attach version
        List<AttachmentVersion> listAttachVersion = attactVersionService.findAllByDocId(docId);
        docOut.setListAttachVersion(listAttachVersion);
        // Set list comment
        //docOut.setListComments(docOutCommentService.getListByDocId(docId));

        //set read
        setRead(docId, BussinessCommon.getUserId(), getUserOrListVanThuBan());

        //set forward status
        docOut.setCanForward(wasForwarded(docId, BussinessCommon.getUserId()));
        docOut.setCanAddUser(canAddUser(docId, BussinessCommon.getUserId()));
        return docOut;
    }

    private void setRead(Long docId, Long userId, List<Long> userIds) {
        List<DocumentOutProcess> pList = docOutProcessService.findByUserRelatedAndDocId(docId, userIds);
        if (!pList.isEmpty()) {
            pList.forEach(i -> {
                if (!Boolean.TRUE.equals(i.getRead())) {
                    i.setRead(true);
                    docOutProcessService.save(i);
                }
            });
        }

        objReadService.setRead(userId, docId, DocumentTypeEnum.VAN_BAN_DI, true);
    }

    public Boolean setRead(List<Long> docIds, Long userId) {
        objReadService.setRead(userId, docIds, DocumentTypeEnum.VAN_BAN_DI, true);
        return true;
    }

    public void validatePermission(long docId) {
        DocumentOut doc = findByDocId(docId);
        validatePermission(doc);
    }

    public void validatePermission(DocumentOut doc) {
        if (!checkPermission(doc)) {
            throw new RestExceptionHandler(Message.NO_PERMISSION);
        }
    }

    public boolean checkPermission(Long docId) {
        DocumentOut doc = findByDocId(docId);
        return checkPermission(doc);
    }

    public boolean checkPermission(DocumentOut doc) {
        Set<Long> docInChecked = new HashSet<>();
        Set<Long> docOutChecked = new HashSet<>();
        Set<Long> taskChecked = new HashSet<>();
        Set<Long> folderChecked = new HashSet<>();
        return checkPermission(doc, docInChecked, docOutChecked, taskChecked, folderChecked);
    }

    public boolean checkPermission(Long docId, Set<Long> docInChecked, Set<Long> docOutChecked, Set<Long> taskChecked, Set<Long> folderChecked) {
        Optional<DocumentOut> doc = docOutRepo.findById(docId);
        if (doc.isPresent()) {
            return checkPermission(doc.get(), docInChecked, docOutChecked, taskChecked, folderChecked);
        }
        return false;
    }

    public boolean checkPermission(DocumentOut doc, Set<Long> docInChecked, Set<Long> docOutChecked, Set<Long> taskChecked, Set<Long> folderChecked) {

        // Check calendar join
        if (calendarService.isMemberRelatedDoc(getUserOrListVanThuBan(), doc.getId(), DocumentTypeEnum.VAN_BAN_DI)) {
            return true;
        }

        if (!docOutChecked.contains(doc.getId())) {
            docOutChecked.add(doc.getId());
            if (checkPermission1(doc)) {
                return true;
            }
        }
        //Check permission vb den lien quan
        for (DocumentInOut docOut : doc.getListRelateDoc()) {
            if (!docInChecked.contains(docOut.getDocIn().getId())
                    && docService.checkPermission(docOut.getDocIn(), docInChecked, docOutChecked, taskChecked, folderChecked)) {
                return true;
            }
        }
        //Check công việc giải quyết vb di => Công việc trả lời
        List<Task> listRelateTask = taskService.getListTaskByDocIdAndDocType(doc.getId(), false);
        for (Task t : listRelateTask) {
            if (!taskChecked.contains(t.getId()) && taskService.checkPermission(t, docInChecked, docOutChecked, taskChecked, folderChecked)) {
                return true;
            }
        }
        //Check vb di giai quyet cong viec => công việc liên quan
        for (DocumentOutTask dt : doc.getListDocumentOutTask()) {
            if (!taskChecked.contains(dt.getTaskId()) && taskService.checkPermission(dt.getTask(), docInChecked, docOutChecked, taskChecked, folderChecked)) {
                return true;
            }
        }
        // Check hồ sơ chứa văn bản
        return hsFolderService.checkPermissionByDocId(doc.getId(), DocumentTypeEnum.VAN_BAN_DI, docInChecked, docOutChecked, taskChecked, folderChecked, false);
    }

    public boolean checkPermission1(DocumentOut doc) {
        // create by
        if (rService.isSupervisor(BussinessCommon.getUser()) || BussinessCommon.getUserId().equals(doc.getCreateBy())) {
            return true;
        }

        if ((doc.getStatus() == DocumentStatusEnum.DU_THAO || doc.getStatus() == DocumentStatusEnum.BI_TRA_LAI
                || doc.getStatus() == DocumentStatusEnum.THU_HOI_XL) && docOutProcessService.existUserInProcessByDocIdAndActive(getUserOrListVanThuBan(), doc.getId(), true,
                BussinessCommon.getClientId())) {
            return true;
        }
        // Trạng thái Đang xử lý thì các users tham gia luồng được quyền xem
        if ((doc.getStatus() == DocumentStatusEnum.DANG_XU_LY) && docOutProcessService.existUserInProcessByDocIdAndActive(getUserOrListVanThuBan(), doc.getId(), true,
                BussinessCommon.getClientId())) {
            return true;
        }
        // Trạng thái Chờ ban hành thì các users tham gia luồng và văn thư cùng tổ chức được quyền xem
        if (doc.getStatus() == DocumentStatusEnum.CHO_BAN_HANH) {
            // Check user tham gia luồng
            // Check chức năng văn thư
            if (docOutProcessService.existUserInProcessByDocIdAndActive(getUserOrListVanThuBan(), doc.getId(), true,
                    BussinessCommon.getClientId()) || userService.isVanThuVBDiByOrg(BussinessCommon.getUser(), doc.getOrgIssuedId())) {
                return true;
            }
        }
        // Trạng thái Đã ban hành thì các users tham gia luồng, văn thư cùng tổ chức và users nhận để biết được quyền xem
        if (doc.getStatus() == DocumentStatusEnum.DA_BAN_HANH) {
            // Người kí
            if (!StringUtils.isNullOrEmpty(doc.getListSignersName())) {
                for (String i : doc.getListSignersName().split(",")) {
                    if (BussinessCommon.getUser().getFullName().equals(i.trim())) {
                        return true;
                    }
                }
            }

            if (!StringUtils.isNullOrEmpty(doc.getListSignerIds())) {
                for (String i : doc.getListSignerIds().split(",")) {
                    if (BussinessCommon.getUserId().toString().equals(i.trim())) {
                        return true;
                    }
                }
            }


            // Check user tham gia luồng

            // Check chức năng văn thư


            // Check nhận để biết
            //Check chức năng thu hồi
            if (docOutProcessService.existUserInProcessByDocIdAndActive(getUserOrListVanThuBan(), doc.getId(), true,
                    BussinessCommon.getClientId()) || (clericalOrg ? clericalOrgService.isClericalOrg(BussinessCommon.getUserId(), doc.getOrgIssuedId())
                    : userService.isVanThuVBDiByOrg(BussinessCommon.getUser(), doc.getOrgIssuedId())) || checkUserNhanDeBiet(doc.getId()) || roleService.checkCanRetake(doc.getOrgIssuedId(), DocumentTypeEnum.VAN_BAN_DI)) {
                return true;
            }
        }
        // Trạng thái thu hồi ban hành có quyền thu hồi ban hành có quyền xem.
        if ((doc.getStatus() == DocumentStatusEnum.THU_HOI_BH) && roleService.checkCanRetake(doc.getOrgIssuedId(), DocumentTypeEnum.VAN_BAN_DI)) {
            return true;
        }
        return false;
    }

    private boolean checkUserNhanDeBiet(Long docId) {
        // Get list ids user nhận để biết
        List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdAndType(BussinessCommon.getClientId(),
                docId, null);
        List<Long> userIds = getUserIdReceiveByDocId(receiveIdList, true);
        return userIds.contains(BussinessCommon.getUserId());
    }

    private List<DocumentReceive> getDataDrList(List<DocumentReceive> drList, Long id) {
        if (drList != null && !drList.isEmpty()) {
            List<Organization> orgNameList = orgService.getOrgByIdList(null, true);
            List<User> userNameList = userService.findByIds(null, true);
            String[] codes = {Constant.CAT_POSITION};
            List<Category> catList = catService.getListCategoriesByCode(codes);
            drList.stream().forEach(dr -> {
                if (Constant.RECEIVER_TYPE_USER.equals(dr.getType())) { // for choose person
                    Optional<User> u = userNameList.stream().filter(i -> i.getId().equals(dr.getReceiveId()))
                            .findFirst();
                    if (u.isPresent()) {
                        dr.setFullName(u.get().getFullName());
                        dr.setOrgId(u.get().getOrg());
                        Optional<Category> c = catList.stream().filter(j -> j.getId().equals(u.get().getPosition()))
                                .findFirst();
                        if (c.isPresent()) {
                            dr.setPositionName(c.get().getName());
                        }
                    }

                    Optional<Organization> o = orgNameList.stream().filter(i -> i.getId().equals(dr.getOrgId()))
                            .findFirst();
                    if (o.isPresent()) {
                        dr.setOrgName(o.get().getName());
                    }

                    dr.setUserId(dr.getReceiveId());
                } else { // for choose org
                    Optional<Organization> o = orgNameList.stream().filter(i -> i.getId().equals(dr.getReceiveId()))
                            .findFirst();
                    if (o.isPresent()) {
                        dr.setOrgName(o.get().getName());
                    }

                    Optional<User> u = userNameList.stream()
                            .filter(i -> i.getOrg().equals(dr.getReceiveId()) && i.isLead()).findFirst();
                    if (u.isPresent()) {
                        dr.setFullName(u.get().getFullName());
                        dr.setUserId(u.get().getId());
                        Optional<Category> c = catList.stream().filter(j -> j.getId().equals(u.get().getPosition()))
                                .findFirst();
                        if (c.isPresent()) {
                            dr.setPositionName(c.get().getName());
                        }
                    }

                    dr.setOrgId(dr.getReceiveId());
                }
                dr.setDocId(id);
            });
        }
        return drList;
    }

    private List<SignerDto> getListSignersByIds(Long[] listIds) {
        int no = 0;
        List<User> listUsers = userService.findByIds(listIds);
        if (listUsers == null) {
            return null;
        }
        List<SignerDto> listSigners = new ArrayList<>();
        List<Long> listOrgIds = new ArrayList<>();
        List<Long> listPosIds = new ArrayList<>();
        for (User u : listUsers) {
            listOrgIds.add(u.getOrg());
            listPosIds.add(u.getPosition());
        }
        List<Organization> listOrg = orgService.findAllById(listOrgIds);
        Map<Long, String> listOrgName = new HashMap<>();
        for (Organization org : listOrg) {
            listOrgName.put(org.getId(), org.getName());
        }
        List<Category> listPos = catService.findAllById(listPosIds);
        Map<Long, String> listPosName = new HashMap<>();
        for (Category cat : listPos) {
            listPosName.put(cat.getId(), cat.getName());
        }

        for (User u : listUsers) {
            SignerDto s = new SignerDto();
            s.setNo(++no);
            s.setId(u.getId());
            s.setFullName(u.getFullName());
            s.setOrgName(listOrgName.get(u.getOrg()));
            s.setPosition(listPosName.get(u.getPosition()));
            listSigners.add(s);
        }
        return listSigners;
    }

    public boolean requestMultipleSign(List<Long> docIds, Long fromUserId, Long toUserId, Long delegateId, Long nodeId,
                                       String comment) {
        try {
            for (Long docId : docIds) {
                if (!requestSign(docId, fromUserId, toUserId, delegateId, nodeId, comment)) {
                    return false;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        return true;
    }

    @Transactional
    public boolean requestSign(Long docId, Long fromUserId, Long toUserId, Long delegateId, Long nodeId,
                               String comment) {
        docOutCommentService.validCommentLength(comment);
        Long delegatedUserId = null;
        if (delegateId != null) {
            delegatedUserId = delegateService.findByIdAndDate(delegateId, new Date());
            if (delegatedUserId == null) {
                throw new RestExceptionHandler("Không tìm thấy văn bản ủy quyền hợp lệ");
            }
        }
        // Check process status
        DocumentOutHandleStatusEnum[] enums = {DocumentOutHandleStatusEnum.DU_THAO,
                DocumentOutHandleStatusEnum.BI_TRA_LAI};
        DocumentOutProcess input = docOutProcessService
                .findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(true, docId, fromUserId, enums);
        if (input == null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }
        DocumentOut doc = findByDocId(docId);
        if ((!DocumentStatusEnum.DU_THAO.equals(doc.getStatus()) && !DocumentStatusEnum.BI_TRA_LAI.equals(doc.getStatus()) && !DocumentStatusEnum.THU_HOI_XL.equals(doc.getStatus()))) {
            throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
        }
        // Kiểm tra file đính kèm
        if (!docOutAttachService.existAttachmentByTypeAndDocId(AttachmentTypeEnum.DRAFT, docId)) {
            throw new RestExceptionHandler(Message.ATTACHMENT_FILE_NOT_FOUND);
        }
        try {
            // Update doc status and nodeId
            if (delegateId != null) {
                List<Long> listSignerIds = Arrays.asList(BussinessCommon.castStringToLongArray(doc.getListSignerIds()));
                if (listSignerIds.contains(toUserId)) {
                    doc.setListSignerIds(doc.getListSignerIds() + "," + delegatedUserId);
                }
                //				// check if exist delegate -> inactive process delegate
                //				DocumentOutProcess delegateProcess = docOutProcessService.findByDelegaterAndDocId(docId,
                //						delegatedUserId);
                //				if (delegateProcess != null) {
                //					delegateProcess.setActive(false);
                //					dopRepo.save(delegateProcess);
                //				}
            }
            updateNodeIdAndStatus(doc, nodeId, DocumentStatusEnum.DANG_XU_LY);
            enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.CHO_Y_KIEN,
                    DocumentOutHandleStatusEnum.DA_Y_KIEN};
            docOutProcessService.setActiveByDocIdAndUserIdAndhandleStatus(false, docId, toUserId, enums);
            // Add process
            input.setHandleStatus(DocumentOutHandleStatusEnum.DA_TRINH_KY);
            docOutProcessService.save(input);
            docOutProcessService.saveOrUpdate(docId, toUserId, toUserId, null, delegateId,
                    DocumentOutHandleStatusEnum.CHO_XU_LY, nodeId);
            // Add tracking
            docOutTrackingService.save(docId, fromUserId, toUserId, DocumentOutTrackingEnum.TRANSFER);
            docOutTrackingService.save(docId, toUserId, null, DocumentOutTrackingEnum.INCOMING);
            // Add comment
            if (!StringUtils.isNullOrEmpty(comment)) {
                docOutCommentService.saveCmt(docId, comment);
            }
            // Delete notification
            notiService.setActiveByUserIdAndDocIdAndDocType(fromUserId, docId, DocumentTypeEnum.VAN_BAN_DI, false);
            // Add notification
            if (delegatedUserId != null) {
                notiService.add(delegatedUserId, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                        NotificationHandleStatusEnum.CHO_XU_LY_UQ, ModuleCodeEnum.DRAFT_HANDLE);
            }
            notiService.add(toUserId, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                    NotificationHandleStatusEnum.CHO_XU_LY, ModuleCodeEnum.DRAFT_HANDLE);
            // Add new version attachment
//			attactVersionService.addNewAttachmentVersion(doc.getAttachments(), true);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        return true;
    }

    public boolean requestComment(Long docId, Long fromUserId, String listToUserId, String comment) {
        docOutCommentService.validCommentLength(comment);
        // Check process status
        DocumentOutHandleStatusEnum[] listStatus = {
                DocumentOutHandleStatusEnum.DU_THAO, DocumentOutHandleStatusEnum.BI_TRA_LAI,
                DocumentOutHandleStatusEnum.CHO_XU_LY};
        DocumentOutProcess input = docOutProcessService
                .findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(true, docId, fromUserId, listStatus);
        if (input == null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }
        DocumentOut doc = findByDocId(docId);
        // Kiểm tra file đính kèm
        if (!docOutAttachService.existAttachmentByTypeAndDocId(AttachmentTypeEnum.DRAFT, docId)) {
            throw new RestExceptionHandler(Message.ATTACHMENT_FILE_NOT_FOUND);
        }
        try {
            Long[] listUser = BussinessCommon.castStringToLongArray(listToUserId);
            // Add process
            List<DocumentOutProcess> listProcess = new ArrayList<>();
            List<Notification> listNotifications = new ArrayList<>();
            for (Long toUserId : listUser) {
                if (!toUserId.equals(fromUserId)) {
                    DocumentOutProcess dop = new DocumentOutProcess();
                    dop.setUserId(toUserId);
                    dop.setHandlerId(toUserId);
                    dop.setDocId(docId);
                    dop.setHandleStatus(DocumentOutHandleStatusEnum.CHO_Y_KIEN);
                    dop.setOrgName(BussinessCommon.getUser().getOrgModel().getName()); // setOrg
                    listProcess.add(dop);

                    Notification noti = new Notification();
                    noti.setUserId(toUserId);
                    noti.setDocId(docId);
                    noti.setDescription(doc.getPreview());
                    noti.setDocType(DocumentTypeEnum.VAN_BAN_DI);
                    noti.setDocStatus(NotificationHandleStatusEnum.CHO_Y_KIEN);
                    noti.setModuleCode(ModuleCodeEnum.DRAFT_HANDLE);
                    listNotifications.add(noti);
                }
            }
            if (!listProcess.isEmpty()) {
                docOutProcessService.saveAll(listProcess);
            }
            // Add comment
            if (!StringUtils.isNullOrEmpty(comment)) {
                docOutCommentService.saveCmt(docId, comment);
            }
            // Add notification
            if (!listNotifications.isEmpty()) {
                notiService.saveAll(listNotifications);
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        return true;
    }

    @Transactional
    public boolean transfer(Long docId, Long toUserId, Long delegateId, Long nodeId, String comment) {
        docOutCommentService.validCommentLength(comment);
        Long currUser = BussinessCommon.getUserId();
        Long delegatedUserId = null;
        if (delegateId != null) {
            delegatedUserId = delegateService.findByIdAndDate(delegateId, new Date());
            if (delegatedUserId == null) {
                throw new RestExceptionHandler(Message.VBUQ_NOT_FOUND);
            }
        }
        // Check process status
        DocumentOutHandleStatusEnum[] enums = {DocumentOutHandleStatusEnum.CHO_XU_LY,
                DocumentOutHandleStatusEnum.BI_TRA_LAI};
        DocumentOutProcess input = docOutProcessService.findByUserIdOrDelegateId(docId, getUserOrListVanThuBan(), enums);
        if (input == null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }
        Long fromUserId = input.getUserId();
        boolean delegate = false;
        // Check ủy quyền
        if (!getUserOrListVanThuBan().contains(fromUserId)) {
            delegate = true;
            delegateService.checkDelegate(fromUserId, currUser);
        }
        DocumentOut doc = findByDocId(docId);
        if (!DocumentStatusEnum.DANG_XU_LY.equals(doc.getStatus()) && !DocumentStatusEnum.BI_TRA_LAI.equals(doc.getStatus()) && !DocumentStatusEnum.DA_BAN_HANH.equals(doc.getStatus())) {
            throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
        }
        //Check vào sổ văn bản
//		doc.setStatus(DocumentStatusEnum.DA_BAN_HANH);
        if (!DocumentStatusEnum.DA_BAN_HANH.equals(doc.getStatus())) {
            validateImportDocBook(doc);
        }
        try {
            // Update nodeId
            if (delegateId != null) {
                List<Long> listSignerIds = Arrays.asList(BussinessCommon.castStringToLongArray(doc.getListSignerIds()));
                if (listSignerIds.contains(toUserId)) {
                    doc.setListSignerIds(doc.getListSignerIds() + "," + delegatedUserId);
                }
            }
            updateNodeId(doc, nodeId);
            // Add process for fromUser
            input.setHandlerId(currUser);
            input.setHandleStatus(DocumentOutHandleStatusEnum.DA_XU_LY);
            docOutProcessService.save(input);
            if (delegate) {
                // Add process for currentUser
                //				docOutProcessService.save(docId, currUser, currUser, fromUserId,
                //						DocumentOutHandleStatusEnum.DA_XU_LY_UQ);
                docOutProcessService.saveOrUpdate(docId, currUser, currUser, fromUserId, null,
                        DocumentOutHandleStatusEnum.DA_XU_LY_UQ, null);
            }
            // Check process of toUser
            enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.CHO_Y_KIEN,
                    DocumentOutHandleStatusEnum.DA_Y_KIEN, DocumentOutHandleStatusEnum.DA_XU_LY};
            docOutProcessService.setActiveByDocIdAndUserIdAndhandleStatus(false, docId, toUserId, enums);
            // Add process for toUser
            docOutProcessService.saveOrUpdate(docId, toUserId, toUserId, null, delegateId,
                    DocumentOutHandleStatusEnum.CHO_XU_LY, nodeId);
            // Add tracking
            docOutTrackingService.update(docId, fromUserId, toUserId, currUser, DocumentOutTrackingEnum.TRANSFER);
            docOutTrackingService.save(docId, toUserId, null, DocumentOutTrackingEnum.INCOMING);
            //			// trong trường hợp người ủy quyền tự xử lý -> mở lại record đã ủy quyền
            //			DocumentOutProcess delegateProcess = docOutProcessService.findByDelegaterAndDocId(docId, currUser, null,
            //					null);
            //			if (delegateProcess != null) {
            //				delegateProcess.setActive(true);
            //				dopRepo.save(delegateProcess);
            //			}

            // Add comment
            if (!StringUtils.isNullOrEmpty(comment)) {
                docOutCommentService.saveCmt(docId, comment);
            }
            // Check process of fromUser
            enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.CHO_Y_KIEN,
                    DocumentOutHandleStatusEnum.DA_Y_KIEN};
            docOutProcessService.setActiveByDocIdAndUserIdAndhandleStatus(false, docId, fromUserId, enums);
            // Delete notification
            if (input.getDelegateId() != null) {
                notiService.setActiveByUserIdAndDocIdAndDocType(input.getDelegate().getToUserId(), docId,
                        DocumentTypeEnum.VAN_BAN_DI, false);
            }
            notiService.setActiveByUserIdAndDocIdAndDocType(fromUserId, docId, DocumentTypeEnum.VAN_BAN_DI, false);
            // Add notification
            if (delegatedUserId != null) {
                notiService.add(delegatedUserId, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                        NotificationHandleStatusEnum.CHO_XU_LY_UQ, ModuleCodeEnum.DRAFT_HANDLE);
            }
            notiService.add(toUserId, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                    NotificationHandleStatusEnum.CHO_XU_LY, ModuleCodeEnum.DRAFT_HANDLE);
            // Add new version attachment
//			attactVersionService.addNewAttachmentVersion(doc.getAttachments(), false);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        return true;
    }

    @Transactional
    public Long finish(Long docId, Long nodeId, String comment) {
        Long currUser = BussinessCommon.getUserId();
        docOutCommentService.validCommentLength(comment);
        Long cmtId = 0L;
        // Check process status
        DocumentOutHandleStatusEnum[] enums = {DocumentOutHandleStatusEnum.DU_THAO,
                DocumentOutHandleStatusEnum.CHO_XU_LY, DocumentOutHandleStatusEnum.BI_TRA_LAI};
        //		DocumentOutProcess input = docOutProcessService.findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(true,
        //				docId, currUser, enums);
        DocumentOutProcess input = docOutProcessService.findByUserIdOrDelegateId(docId, getUserOrListVanThuBan(), enums);
        if (input == null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }
        Long fromUserId = input.getUserId();
        boolean delegate = false;
        if (!currUser.equals(fromUserId)) {
            delegate = true;
            // Check ủy quyền
            delegateService.checkDelegate(fromUserId, currUser);
        }
        DocumentOut doc = findByDocId(docId);
        if (DocumentOutHandleStatusEnum.DU_THAO.equals(input.getHandleStatus())) {
            if ((!DocumentStatusEnum.DU_THAO.equals(doc.getStatus()) && !DocumentStatusEnum.BI_TRA_LAI.equals(doc.getStatus()) && !DocumentStatusEnum.THU_HOI_XL.equals(doc.getStatus()))) {
                throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
            }
        } else if (!DocumentStatusEnum.DANG_XU_LY.equals(doc.getStatus()) && !DocumentStatusEnum.BI_TRA_LAI.equals(doc.getStatus()) && !DocumentStatusEnum.DA_BAN_HANH.equals(doc.getStatus())) {
            throw new RestExceptionHandler(Message.DOCUMENT_STATUS_NOT_ALLOW);
        }
        // Kiểm tra file đính kèm
        if (!docOutAttachService.existAttachmentByTypeAndDocId(AttachmentTypeEnum.DRAFT, docId)) {
            throw new RestExceptionHandler(Message.ATTACHMENT_FILE_NOT_FOUND);
        }
        //Check vào sổ văn bản
        if (!DocumentStatusEnum.DA_BAN_HANH.equals(doc.getStatus())) {
            validateImportDocBook(doc);
        }
        try {
            // Update doc status
            if (Boolean.TRUE.equals(doc.getAutoIssued())) {
                autoIssued(doc, nodeId, true);
            } else {
                updateNodeIdAndStatusAndOrgIssued(doc, nodeId, DocumentStatusEnum.CHO_BAN_HANH,
                        BussinessCommon.getUser().getOrg());
                // Delete notification
                notiService.setActiveByUserIdAndDocIdAndDocType(fromUserId, docId, DocumentTypeEnum.VAN_BAN_DI, false);
                // Add notification cho văn thư
                // Lấy danh sách văn thư theo org
                List<Long> listUserIds = userService.getListIdsVanThuVBDiByOrg(BussinessCommon.getOrgId());
                if (listUserIds != null) {
                    List<Notification> listNotifications = new ArrayList<>();
                    for (long id : listUserIds) {
                        Notification noti = new Notification();
                        noti.setUserId(id);
                        noti.setDocId(docId);
                        noti.setDescription(doc.getPreview());
                        noti.setDocType(DocumentTypeEnum.VAN_BAN_DI);
                        noti.setDocStatus(NotificationHandleStatusEnum.CHO_BAN_HANH);
                        noti.setModuleCode(ModuleCodeEnum.DRAFT_ISSUED);
                        listNotifications.add(noti);
                    }
                    notiService.saveAll(listNotifications);
                }
            }
            // Update process status
            input.setHandlerId(currUser);
            input.setHandleStatus(DocumentOutHandleStatusEnum.DA_XU_LY);
            docOutProcessService.save(input);
            if (delegate) {
                // Add prcess for currentUser
                docOutProcessService.saveOrUpdate(docId, currUser, currUser, fromUserId, null,
                        DocumentOutHandleStatusEnum.DA_XU_LY_UQ, null);
            }
            // Add tracking
            docOutTrackingService.update(docId, fromUserId, null, currUser, DocumentOutTrackingEnum.FINISH);
            // Add comment
            if (!StringUtils.isNullOrEmpty(comment)) {
                cmtId = docOutCommentService.saveCmt(docId, comment).getId();
            }
            // Check process of fromUser
            enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.CHO_Y_KIEN,
                    DocumentOutHandleStatusEnum.DA_Y_KIEN};
            docOutProcessService.setActiveByDocIdAndUserIdAndhandleStatus(false, docId, fromUserId, enums);
            // Delete notification
            if (input.getDelegateId() != null) {
                notiService.setActiveByUserIdAndDocIdAndDocType(input.getDelegate().getToUserId(), docId,
                        DocumentTypeEnum.VAN_BAN_DI, false);
            }
            notiService.setActiveByUserIdAndDocIdAndDocType(fromUserId, docId, DocumentTypeEnum.VAN_BAN_DI, false);
            // Hoàn thành văn bản phúc đáp
            if (autoFinish && Boolean.TRUE.equals(doc.getReplyDoc())) {
                Long[] listIds = BussinessCommon.castStringToLongArray(doc.getReplyDocIds());
                docInProcessService.updateAuto(Arrays.asList(listIds));
            }
            if (doc.shouldEncrypt()) {
                doaService.encrypt(doc);
                docOutRepo.save(doc);
            }
            // Add new version attachment
            attactVersionService.addNewAttachmentVersion(doc.getAttachments(), false);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        return cmtId;
    }

    @Transactional
    public Long retake(Long docId, String comment) {
        docOutCommentService.validCommentLength(comment);
        Long cmtId = 0L;
        User user = BussinessCommon.getUser();
        Long userId = user.getId();
        DocumentOut doc = findByDocId(docId);
        List<Documents> documents = docOutRepo.findByListDocumentsAndActiveAndDocOutId(docId, BussinessCommon.getClientId());
        if (doc.getStatus().equals(DocumentStatusEnum.DA_BAN_HANH)) {
            if (!roleService.checkCanRetake(doc.getOrgIssuedId(), DocumentTypeEnum.VAN_BAN_DI)) {
                throw new RestExceptionHandler(Message.NO_PERMISSION);
            }
            try {
                // Update status and nodeId
                doc.setStatus(DocumentStatusEnum.THU_HOI_BH);
                doc.setNodeId(null);
                docOutRepo.save(doc);
                if (documents != null || documents.size() != 0) {
                    for (Documents docu : documents) {
                        docu.setStatus(DocumentStatusEnum.RETAKE_DOC);
                        docService.save(docu);
                    }

                }
                // Xóa tất cả thông báo trước đó
                notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DI, false);
                // Add thông báo cho những người liên quan đến văn bản
                List<Long> listUserIds;
                // Danh sách người tham gia xử lý
                listUserIds = docOutProcessService.getListUserIdByDocIdAndActive(docId, true,
                        BussinessCommon.getClientId());
                // Cần lấy ra danh sách người nhận để biết
                List<DocumentReceive> receiveIdList = drService
                        .findByClientIdAndDocIdAndType(BussinessCommon.getClientId(), docId, null);
                List<Long> listOrgIds = new ArrayList<>();
                for (DocumentReceive dr : receiveIdList) {
                    if (Constant.RECEIVER_TYPE_USER.equals(dr.getType())) {
                        listUserIds.add(dr.getReceiveId());
                    } else {
                        listOrgIds.add(dr.getReceiveId());
                    }
                }
                List<Long> listUserIdByOrgs = userService.getListLeadUserIdByOrg(listOrgIds);
                if (listUserIdByOrgs != null) {
                    listUserIds.addAll(listUserIdByOrgs);
                }
                String description = "Văn bản số " + doc.getNumberOrSign() + " đã bị thu hồi";
                // Add notification
                notiService.addAll(listUserIds, docId, description, DocumentTypeEnum.VAN_BAN_DI,
                        NotificationHandleStatusEnum.DA_THU_HOI_BH, ModuleCodeEnum.DOC_OUT_RETAKE);
                // Thêm comment và tracking
                if (!StringUtils.isNullOrEmpty(comment)) {
                    DocumentOutComment cmt = docOutCommentService.saveCmt(docId, comment);
                    cmtId = cmt.getId();
                    docOutTrackingService.save(docId, DocumentOutTrackingEnum.THU_HOI_BH, cmt.getId());
                } else {
                    docOutTrackingService.save(docId, DocumentOutTrackingEnum.THU_HOI_BH, null);
                }
            } catch (Exception e) {
                e.printStackTrace();
                throw new RestExceptionHandler(Message.ACTION_FAILED);
            }
            return cmtId;
        }
        if (!doc.getStatus().equals(DocumentStatusEnum.DANG_XU_LY)) {
            throw new RestExceptionHandler(Message.RETAKE_NOT_ALLOWED);
        }
        if (!doc.getPersonEnterId().equals(userId)) {
            throw new RestExceptionHandler(Message.PERSON_NOT_ALLOWED);
        }
        try {
            // Update status and nodeId
            doc.setStatus(DocumentStatusEnum.THU_HOI_XL);
            doc.setNodeId(null);
            docOutRepo.save(doc);
            // Deactive tất cả records trong process có docId = docId.
            docOutProcessService.deactiveByDocId(docId);
            // Add process cho người thu hồi
            docOutProcessService.save(docId, userId, userId, null, DocumentOutHandleStatusEnum.DU_THAO);
            // Thêm comment và tracking
            if (!StringUtils.isNullOrEmpty(comment)) {
                DocumentOutComment cmt = docOutCommentService.saveCmt(docId, comment);
                cmtId = cmt.getId();
                docOutTrackingService.save(docId, DocumentOutTrackingEnum.RETAKE, cmt.getId());
            } else {
                docOutTrackingService.save(docId, DocumentOutTrackingEnum.RETAKE, null);
            }
            // Xóa tất cả thông báo trước đó
            notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DI, false);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        return cmtId;
    }

    @Transactional
    public boolean retakeByStep(Long docId, String comment, MultipartFile[] files) {
        docOutCommentService.validCommentLength(comment);
        DocumentOut doc = findByDocId(docId);
        doc.setStatus(DocumentStatusEnum.THU_HOI_BH);
//		if (!doc.getStatus().equals(DocumentStatusEnum.DANG_XU_LY)) {
//			throw new RestExceptionHandler(Message.RETAKE_NOT_ALLOWED);
//		}

        // Check process status
        DocumentOutHandleStatusEnum[] enums = {DocumentOutHandleStatusEnum.CHO_XU_LY, DocumentOutHandleStatusEnum.DU_THAO,
                DocumentOutHandleStatusEnum.BI_TRA_LAI};
        DocumentOutProcess input = docOutProcessService.findFirstByActiveAndDocIdAndHandleStatusInOrderByIdDesc(true, docId, enums);
        if (input == null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }
        Long currUser = BussinessCommon.getUserId();
        Long toUserId = input.getUserId();
        DocumentOutTracking dot = docOutTrackingService.getLastTrackingByDocIdAndToUserIdAndAction(docId, toUserId, DocumentOutTrackingEnum.TRANSFER);
        if (dot == null && doc.getNodeId() != null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        } else if (dot != null) {
            doc.setNodeId(dot.getPreNodeId());
        }
        enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.DA_XU_LY};
        DocumentOutProcess preUserProcess = null;
        if (doc.getNodeId() != null) {
            preUserProcess = docOutProcessService.findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(true, docId, dot.getFromUserId(), enums);
            if (preUserProcess == null) {
                throw new RestExceptionHandler(Message.INVALID_PROCESS);
            }
            Long fromUserId = preUserProcess.getUserId();
            if (!currUser.equals(fromUserId)) {
                // Check ủy quyền
                delegateService.checkDelegate(fromUserId, currUser);
            }
        }


        try {
            // Update nodeId
            //doc.setNodeId(dot.getPreNodeId());
            docOutRepo.save(doc);
            input.setHandlerId(currUser);
            input.setHandleStatus(DocumentOutHandleStatusEnum.BI_THU_HOI);
            input.setActive(false);
            docOutProcessService.save(input);

            // văn bản ban hành trực tiếp sẽ không có luồng. bỏ qua và cho thu hồi
            if (doc.getNodeId() != null) {
                // Update process cho người thu hồi
                preUserProcess.setHandleStatus(DocumentOutHandleStatusEnum.CHO_XU_LY);
                docOutProcessService.save(preUserProcess);
                if (preUserProcess.getUserId() != null && !preUserProcess.getUserId().equals(preUserProcess.getHandlerId())) {
                    enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.DA_XU_LY_UQ};
                    DocumentOutProcess delegateProcess = docOutProcessService.findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(true, docId, preUserProcess.getHandlerId(), enums);
                    if (delegateProcess != null) {
                        delegateProcess.setActive(false);
                        docOutProcessService.save(delegateProcess);
                    }
                }
            }


            // Thêm comment và tracking
            if (!StringUtils.isNullOrEmpty(comment)) {
                DocumentOutComment cmt = docOutCommentService.saveCmt(docId, comment);
                docOutAttachService.addListAttachment(files, "comment", cmt.getId());
                docOutTrackingService.save(docId, DocumentOutTrackingEnum.RETAKE, cmt.getId());
            } else {
                docOutTrackingService.save(docId, DocumentOutTrackingEnum.RETAKE, null);
            }
            // Xóa tất cả thông báo trước đó
            if (input.getDelegateId() != null) {
                notiService.setActiveByUserIdAndDocIdAndDocType(input.getDelegate().getToUserId(), docId,
                        DocumentTypeEnum.VAN_BAN_DI, false);
            }
            if (doc.getNodeId() != null) {
                notiService.setActiveByUserIdAndDocIdAndDocType(dot.getToUserId(), docId, DocumentTypeEnum.VAN_BAN_DI,
                        false);
            }

        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        return true;
    }

    @Transactional
    public Long restore(Long docId, String comment) {
        docOutCommentService.validCommentLength(comment);
        Long cmtId = 0L;
        DocumentOut doc = findByDocId(docId);
        if (doc.getStatus().equals(DocumentStatusEnum.THU_HOI_BH)) {
            if (!roleService.checkCanRetake(doc.getOrgIssuedId(), DocumentTypeEnum.VAN_BAN_DI)) {
                throw new RestExceptionHandler(Message.NO_PERMISSION);
            }
            try {
                // Update status and nodeId
                doc.setStatus(DocumentStatusEnum.DA_BAN_HANH);
                doc.setNodeId(null);
                docOutRepo.save(doc);
                // Xóa tất cả thông báo trước đó
                notiService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DI, false);
                // Thêm comment và tracking
                if (!StringUtils.isNullOrEmpty(comment)) {
                    DocumentOutComment cmt = docOutCommentService.saveCmt(docId, comment);
                    cmtId = cmt.getId();
                    docOutTrackingService.save(docId, DocumentOutTrackingEnum.KHOI_PHUC_BH, cmt.getId());
                } else {
                    docOutTrackingService.save(docId, DocumentOutTrackingEnum.KHOI_PHUC_BH, null);
                }
            } catch (Exception e) {
                e.printStackTrace();
                throw new RestExceptionHandler(Message.ACTION_FAILED);
            }
        }
        return cmtId;
    }

    @Transactional
    public Long reject(Long docId, String comment) {
        docOutCommentService.validCommentLength(comment);
        Long cmtId = 0L;
        // Check process status
        DocumentOutHandleStatusEnum[] enums = {DocumentOutHandleStatusEnum.CHO_XU_LY,
                DocumentOutHandleStatusEnum.BI_TRA_LAI};
        DocumentOutProcess input = docOutProcessService.findFirstByActiveAndDocIdAndHandleStatusInOrderByIdDesc(true,
                docId, enums);
        if (input == null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }
        boolean delegate = false;
        Long currUser = BussinessCommon.getUserId();
        Long fromUserId = input.getUserId();
        if (!currUser.equals(fromUserId)) {
            delegate = true;
            // Check ủy quyền
            delegateService.checkDelegate(fromUserId, currUser);
        }
        DocumentOutTracking dot = docOutTrackingService.getLastTrackingByDocIdAndToUserIdAndAction(docId, fromUserId,
                DocumentOutTrackingEnum.TRANSFER);
        if (dot == null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }
        enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.DA_XU_LY,
                DocumentOutHandleStatusEnum.DA_TRINH_KY};
        DocumentOutProcess preUserProcess = docOutProcessService
                .findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(true, docId, dot.getFromUserId(),
                        enums);
        DocumentOut doc = findByDocId(docId);
        try {
            // Update status and nodeId
            if (doc.getPersonEnterId().equals(dot.getFromUserId())) {
                doc.setStatus(DocumentStatusEnum.BI_TRA_LAI);
            }
            doc.setNodeId(dot.getPreNodeId());
            docOutRepo.save(doc);
            input.setHandlerId(currUser);
            input.setHandleStatus(DocumentOutHandleStatusEnum.DA_TRA_LAI);
            //			input.setActive(false);
            docOutProcessService.save(input);
            if (delegate) {
                // Add process for currentUser
                docOutProcessService.save(docId, currUser, currUser, fromUserId,
                        DocumentOutHandleStatusEnum.DA_TRA_LAI_UQ);
            }
            if (preUserProcess != null) {
                if (DocumentOutHandleStatusEnum.DA_XU_LY.equals(preUserProcess.getHandleStatus())) {
                    preUserProcess.setHandleStatus(DocumentOutHandleStatusEnum.BI_TRA_LAI);
                } else {
                    preUserProcess.setHandleStatus(DocumentOutHandleStatusEnum.DU_THAO);
                }
                docOutProcessService.save(preUserProcess);
            } else {
                docOutProcessService.save(docId, dot.getFromUserId(), dot.getFromUserId(), null,
                        DocumentOutHandleStatusEnum.BI_TRA_LAI);
            }
            // Thêm comment và tracking
            if (!StringUtils.isNullOrEmpty(comment)) {
                DocumentOutComment cmt = docOutCommentService.saveCmt(docId, comment);
                cmtId = cmt.getId();
                docOutTrackingService.save(docId, dot.getToUserId(), currUser, DocumentOutTrackingEnum.REJECT,
                        cmt.getId());
            } else {
                docOutTrackingService.save(docId, dot.getToUserId(), currUser, DocumentOutTrackingEnum.REJECT, null);
            }
            // Xóa tất cả thông báo trước đó
            if (input.getDelegateId() != null) {
                notiService.setActiveByUserIdAndDocIdAndDocType(input.getDelegate().getToUserId(), docId,
                        DocumentTypeEnum.VAN_BAN_DI, false);
            }
            notiService.setActiveByUserIdAndDocIdAndDocType(dot.getToUserId(), docId, DocumentTypeEnum.VAN_BAN_DI,
                    false);
            // Add notification
            if (doc.getPersonEnterId().equals(dot.getFromUserId())) {
                notiService.add(dot.getFromUserId(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                        NotificationHandleStatusEnum.BI_TRA_LAI, ModuleCodeEnum.DRAFT_LIST);
            } else {
                if (preUserProcess != null && preUserProcess.getDelegateId() != null) {
                    notiService.add(preUserProcess.getDelegate().getToUserId(), docId, doc.getPreview(),
                            DocumentTypeEnum.VAN_BAN_DI, NotificationHandleStatusEnum.BI_TRA_LAI_UQ,
                            ModuleCodeEnum.DRAFT_HANDLE);
                }
                notiService.add(dot.getFromUserId(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                        NotificationHandleStatusEnum.BI_TRA_LAI, ModuleCodeEnum.DRAFT_HANDLE);
            }
        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        return cmtId;
    }

    public List<DocumentReceiveBasicDto> getListDocumentReceive(Long docId) {
        return drService.findDocumentReceiveDtoByClientIdAndDocIdAndType(BussinessCommon.getClientId(), docId, null);
    }

    public boolean forward(ShowToKnowDto input) {
        docOutCommentService.validCommentLength(input.getComment());
        DocumentOut doc = findByDocId(input.getDocId());
        if (!DocumentStatusEnum.DA_BAN_HANH.equals(doc.getStatus())) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        // Get danh sách người nhận để biết
        List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdAndType(BussinessCommon.getClientId(),
                input.getDocId(), null);
        List<Long> oUserReceiveIds = getUserIdReceiveByDocId(receiveIdList, false);
        if ((!userService.isVanThuVBDiByOrg(BussinessCommon.getUser(), doc.getOrgIssuedId()) && !oUserReceiveIds.contains(BussinessCommon.getUserId()))) {
            throw new RestExceptionHandler(Message.NO_PERMISSION);
        }

        List<DocumentReceive> t = validReceiveId(input.getListReceive(), doc.getId());
        List<DocumentReceive> addList = setDrList(t, doc.getId(), 1);

        if (addList != null && !addList.isEmpty()) {
            // add danh sách nhận để biết
            docReceiveService.saveAll(addList);

            // Cần lấy ra danh sách người nhận để biết để add notification cho nó.
            List<Long> userIds = getUserIdReceiveByDocId(addList, false);

            // Add notification
            notiService.addAll(userIds, doc.getId(), doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                    NotificationHandleStatusEnum.DA_BAN_HANH, ModuleCodeEnum.DOCUMENT_IN_LIST);

            // Save tracking
            docOutTrackingService.saveAll(doc.getId(), BussinessCommon.getUserId(), userIds,
                    DocumentOutTrackingEnum.INCOMING);
        }

        // Add comment
        if (!StringUtils.isNullOrEmpty(input.getComment())) {
            docOutCommentService.saveCmt(doc.getId(), input.getComment());
        }

        transferOutside(doc, addList);

        docReceiveService.setStatus(input.getDocId(), BussinessCommon.getUserId(), DocumentStatusEnum.DONE);
        return true;
    }

    /**
     * transfer document out to outside organization
     *
     * @param doc
     * @param drList
     */
    private void transferOutside(DocumentOut doc, List<DocumentReceive> drList) {
        for (DocumentReceive i : drList) {
            docService.saveFromDocOut(doc, i.getReceiveId(), i.getType());
        }
    }

    public DataInitDto getDataInit(Boolean isInternalDocument) {
        DataInitDto initDto = new DataInitDto();
        String[] codes = {Constant.CAT_DOC_TYPE, Constant.CAT_FIELD, Constant.CAT_SECURE, Constant.CAT_URENT};
        List<CategoryType> catTypeList = catTypeService.getListCategoriesTypeByCode(codes);
        List<Category> catList = catService.getListCategoriesByCode(codes);

        Long bookType = isInternalDocument ? Constant.BOOK_DOC_VB_DI_NOI_BO : Constant.BOOK_DOC_VB_DI;

        // get next bookId
        initDto.setBookId(dbookService.getMaxCurrentNumberByBookType(bookType) + 1);

        // get book list
        List<DocumentBook> dbList = dbookService.findByActiveAndBookType(bookType, true);

        // start node
        initDto.setNode(null);

        initDto.setDocTypeCategories(catService.getCategoryInitDtoByCode(catList, catTypeList, Constant.CAT_DOC_TYPE));
        initDto.setUrgentCategories(catService.getCategoryInitDtoByCode(catList, catTypeList, Constant.CAT_URENT));
        initDto.setSecurityCategories(catService.getCategoryInitDtoByCode(catList, catTypeList, Constant.CAT_SECURE));
        initDto.setBookCategories(dbookService.castToCategoryInitDto(dbList));
        initDto.setDocFieldCategories(catService.getCategoryInitDtoByCode(catList, catTypeList, Constant.CAT_FIELD));

        // org && user
        User user = BussinessCommon.getUser();
        initDto.setUserCreateName(user.getFullName());

        Organization org = orgService.validOrgId(user.getOrg());
        initDto.setOrgCreateName(org.getName());

        initDto.setCheckRoleLibrarian(userService.isVanThuVBDen(user));
        return initDto;
    }

    public SearchDocFormDto getSearchDocForm() {
        String[] codesDVN = {Constant.CAT_ORG_OTHER};
        SearchDocFormDto dto = new SearchDocFormDto();
        dto.setOrgIssuedCategories(catService.castToCategoryInitDto(catService.getListCategoriesByCode(codesDVN)));
        return dto;
    }

    public ListObjectDto<DocOutSignerDto> getListDraft(Boolean important, String text, String numberOrSign,
                                                       String preview, Date startCreate, Date endCreate, Long docType, Long docFieds, Pageable page) {
        User u = BussinessCommon.getUser();
        DocumentStatusEnum[] codes = {DocumentStatusEnum.DU_THAO, DocumentStatusEnum.BI_TRA_LAI,
                DocumentStatusEnum.THU_HOI_XL};
        DocumentOutHandleStatusEnum[] handleStatus = {DocumentOutHandleStatusEnum.DU_THAO};
        Page<DocumentOut> docOutPage = docOutRepo.getListDraft(text, important, DocumentTypeEnum.VAN_BAN_DI, null,
                u.getId(), numberOrSign, preview, null, startCreate, endCreate, null, null, docType, docFieds, null,
                codes, BussinessCommon.getClientId(), null, handleStatus, page);
        return BussinessCommon.paging(castToDocDto(docOutPage, page, docOutPage.getTotalElements()));
    }

    public Page<DocOutSignerDto> castToDocDto(Page<DocumentOut> domainPage, Pageable page, long size) {
        if (BussinessCommon.isEmptyPage(domainPage)) {
            return null;
        }
        List<DocOutSignerDto> dtoList = new ArrayList<>();
        long no = domainPage.getPageable().getOffset();
        // Get list document user
        List<Long> idList = domainPage.getContent().stream().map(DocumentOut::getId).collect(Collectors.toList());
        List<DocumentUser> listDU = docUserService.findByDocTypeAndDocIdInAndUserId(DocumentTypeEnum.VAN_BAN_DI, idList,
                BussinessCommon.getUserId());
        Map<Long, DocumentUser> mapPOfDU = new HashMap<>();
        for (DocumentUser du : listDU) {
            mapPOfDU.put(du.getDocId(), du);
        }

        for (DocumentOut d : domainPage.getContent()) {
            DocOutSignerDto dto = new DocOutSignerDto(d);
            dto.setNo(++no);
            if (mapPOfDU.get(d.getId()) != null) {
                dto.setImportant(mapPOfDU.get(d.getId()).getImportant());
            } else {
                dto.setImportant(false);
            }
            dtoList.add(dto);
        }

        return new PageImpl<>(dtoList, page, size);
    }

    public Page<DocOutIssuedDto> castToDocIssuedDto(Page<DocumentOut> domainPage, Pageable page, long size,
                                                    boolean isAll) {
        if (BussinessCommon.isEmptyPage(domainPage)) {
            return null;
        }
        List<DocOutIssuedDto> dtoList = castToDocIssuedDtoByList(domainPage.getContent(), page, isAll);
        return new PageImpl<>(dtoList, page, size);
    }

    public List<DocOutIssuedDto> castToDocIssuedDtoByList(List<DocumentOut> docOutList, Pageable page, boolean isAll) {
        List<DocOutIssuedDto> dtoList = new ArrayList<>();
        List<Long> idList = docOutList.stream().map(DocumentOut::getId).collect(Collectors.toList());
        Map<Long, Boolean> objReadMap = objReadService.getObjReadMap(BussinessCommon.getUserId(), idList, DocumentTypeEnum.VAN_BAN_DI);
        List<DocumentOutProcess> pList = docOutProcessService.findByDocIdAndClientIdLastedAndUser(idList);
        // Get list document user
        List<DocumentUser> listDU = docUserService.findByDocTypeAndDocIdInAndUserId(DocumentTypeEnum.VAN_BAN_DI, idList,
                BussinessCommon.getUserId());
        Map<Long, DocumentUser> mapPOfDU = new HashMap<>();
        for (DocumentUser du : listDU) {
            mapPOfDU.put(du.getDocId(), du);
        }

        Long[] ids = null;
        List<DocumentReceiveDto> drList = null;
        if (idList != null) {
            ids = idList.stream().toArray(Long[]::new);
            drList = drService.getFullNameByDocId(ids);
        }
        long no = page != null ? page.getOffset() : 0;
        for (DocumentOut d : docOutList) {
            DocOutIssuedDto dto = new DocOutIssuedDto(d);
            dto.setNo(++no);
            if (drList != null && !drList.isEmpty()) {
                dto.setShowToKnow(drService.getFullNameByDocId(d.getId(), drList));
            }

            if (mapPOfDU.get(d.getId()) != null) {
                dto.setImportant(mapPOfDU.get(d.getId()).getImportant());
            } else {
                dto.setImportant(false);
            }

            if (isAll) {
                Optional<DocumentOutProcess> p = pList.stream().filter(
                                i -> d.getId().equals(i.getDocId()) && i.getUserId().equals(BussinessCommon.getUserId()))
                        .findFirst();
                if (p.isPresent()) {
                    dto.setStatusHandleEnum(p.get().getHandleStatus());
                }
            }
            dto.setNumberInBook(d.getNumberInBook());
            dto.setRead(setRead(dto.getDocOutId(), objReadMap));
            dto.setSignerName(d.getSignerName());
            dto.setOrgCreateName(d.getOrgCreateName());
            List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdTypeNotTransfer(BussinessCommon.getClientId(),
                    d.getId());

            dto.setListReceive(getDataDrList(receiveIdList, d.getId()));
            dtoList.add(dto);
        }

        return dtoList;
    }

    private boolean setRead(Long key, Map<Long, Boolean> objReadMap) {
        if (objReadMap.containsKey(key)) {
            return objReadMap.get(key);
        }
        return false;
    }

    public Page<DocOutIssuedDto> castToDocIssuedDtoTrinhKi(Page<DocumentOutDto> domainPage, Pageable page, long size) {
        if (BussinessCommon.isEmptyPage(domainPage)) {
            return null;
        }
        List<DocumentOut> docOutList = domainPage.getContent().stream().map(DocumentOutDto::getDocOut)
                .collect(Collectors.toList());
        List<DocOutIssuedDto> dtoList = new ArrayList<>();
        List<Long> idList = docOutList.stream().map(DocumentOut::getId).collect(Collectors.toList());
        Long[] ids = null;
        List<DocumentReceiveDto> drList = null;
        List<DocumentOutProcess> personHandles = docOutProcessService.findByDocIdAndClientIdLasted(idList);
        // Get list document user
        List<DocumentUser> listDU = docUserService.findByDocTypeAndDocIdInAndUserId(DocumentTypeEnum.VAN_BAN_DI, idList,
                BussinessCommon.getUserId());
        Map<Long, DocumentUser> mapPOfDU = new HashMap<>();
        for (DocumentUser du : listDU) {
            mapPOfDU.put(du.getDocId(), du);
        }

        if (idList != null) {
            ids = idList.stream().toArray(Long[]::new);
            drList = drService.getFullNameByDocId(ids);
        }

        long no = domainPage.getPageable().getOffset();
        for (DocumentOutDto ds : domainPage.getContent()) {
            DocOutIssuedDto dto = new DocOutIssuedDto(ds.getDocOut());
            dto.setNo(++no);
            if (personHandles != null && !personHandles.isEmpty()) {
                Optional<DocumentOutProcess> p = personHandles.stream()
                        .filter(i -> i.getDocId().equals(ds.getDocOut().getId())).findFirst();
                if (p.isPresent()) {
                    dto.setPersonHandle(p.get().getUser().getFullName());
                }
            }

            if (mapPOfDU.get(ds.getDocOut().getId()) != null) {
                dto.setImportant(mapPOfDU.get(ds.getDocOut().getId()).getImportant());
            } else {
                dto.setImportant(false);
            }

            if (drList != null && !drList.isEmpty()) {
                dto.setShowToKnow(drService.getFullNameByDocId(ds.getDocOut().getId(), drList));
            }

            dtoList.add(dto);
        }
        return new PageImpl<>(dtoList, page, size);
    }

    public Page<DocOutDto> getListDocOut(Integer page) {
        Page<DocOutDto> pageDto = docOutRepo.getListDocOut(BussinessCommon.getUser().getUserName(),
                BussinessCommon.getClientId(), DocumentStatusEnum.DA_BAN_HANH, BussinessCommon.castToPageable(page));
        List<DocOutDto> list = pageDto.getContent();
        for (int i = 0; i < list.size(); ++i) {
            DocOutDto dto = list.get(i);
            dto.setNo((int) pageDto.getPageable().getOffset() + i + 1);
        }
        return pageDto;
    }

    public Page<DocOutDto> getListIssued(String retaked, Pageable pageable) {
        User u = BussinessCommon.getUser();
        Page<DocOutDto> pageDto = null;
        if ("true".equals(retaked)) {
            pageDto = docOutRepo.getListIssued(false, null, u.getId(), u.getOrg(), u.getClientId(),
                    DocumentStatusEnum.THU_HOI_BH, pageable);
        } else {
            pageDto = docOutRepo.getListIssued(false, null, u.getId(), u.getOrg(), u.getClientId(),
                    DocumentStatusEnum.DA_BAN_HANH, pageable);
        }

        List<DocOutDto> list = pageDto.getContent();
        for (int i = 0; i < list.size(); ++i) {
            DocOutDto dto = list.get(i);
            dto.setNo((int) pageDto.getPageable().getOffset() + i + 1);
        }
        return pageDto;
    }

    public Page<DocOutDto> getListIssued(String retaked, String text, Pageable pageable) {
        User u = BussinessCommon.getUser();
        Page<DocOutDto> pageDto = null;
        if ("true".equals(retaked)) {
            pageDto = docOutRepo.getListIssued(false, text, u.getId(), u.getOrg(), u.getClientId(),
                    DocumentStatusEnum.THU_HOI_BH, pageable);
        } else {
            pageDto = docOutRepo.getListIssued(false, text, u.getId(), u.getOrg(), u.getClientId(),
                    DocumentStatusEnum.DA_BAN_HANH, pageable);
        }

        List<DocOutDto> list = pageDto.getContent();
        for (int i = 0; i < list.size(); ++i) {
            DocOutDto dto = list.get(i);
            dto.setNo((int) pageDto.getPageable().getOffset() + i + 1);
        }
        return pageDto;
    }

    public Page<DocOutDto> getListIssued(String retaked, String numberOrSign, String orgCreateName, String personEnter,
                                         String preview, Date startCreate, Date endCreate, Date startIssued, Date endIssued, Long docTypeId,
                                         Long docFieldId, Long bookId, Pageable pageable) {
        User u = BussinessCommon.getUser();
        Page<DocOutDto> pageDto = null;
        if ("true".equals(retaked)) {
            pageDto = docOutRepo.getListIssued(false, numberOrSign, orgCreateName, personEnter, preview,
                    startCreate, endCreate, startIssued, endIssued, docTypeId, docFieldId, bookId, u.getId(),
                    u.getOrg(), u.getClientId(), DocumentStatusEnum.THU_HOI_BH, pageable);
        } else {
            pageDto = docOutRepo.getListIssued(false, numberOrSign, orgCreateName, personEnter, preview,
                    startCreate, endCreate, startIssued, endIssued, docTypeId, docFieldId, bookId, u.getId(),
                    u.getOrg(), u.getClientId(), DocumentStatusEnum.DA_BAN_HANH, pageable);
        }

        List<DocOutDto> list = pageDto.getContent();
        for (int i = 0; i < list.size(); ++i) {
            DocOutDto dto = list.get(i);
            dto.setNo((int) pageDto.getPageable().getOffset() + i + 1);
        }
        return pageDto;
    }

    public ListObjectDto<DocOutIssuedDto> getListIssued(Boolean important, Long type, String text, String numberOrSignL,
                                                        String orgCreateNameL, String personEnterL, String previewL, Date startCreateL, Date endCreateL,
                                                        Date startIssuedL, Date endIssuedL, Long docTypeL, Long docFieldsL, Long bookIdL, Boolean isInternalDocument, Pageable page) {
        User u = BussinessCommon.getUser();
        Page<DocumentOut> docOutPage = null;
        DocumentStatusEnum[] codes = new DocumentStatusEnum[1];
        if (type != null && type.longValue() == 2) { // tab vb trinh ki (da ban hanh)
            codes[0] = DocumentStatusEnum.DA_BAN_HANH;
            docOutPage = docOutRepo.getIssuedDocsAdvance(text, important, DocumentTypeEnum.VAN_BAN_DI, null, u.getId(),
                    numberOrSignL, previewL, orgCreateNameL, startCreateL, endCreateL, startIssuedL, endIssuedL,
                    docTypeL, docFieldsL, bookIdL, codes, u.getClientId(), null, null, page);
        } else {

            if (type != null && type.longValue() == 1) { // tab vb trinh ki (da trinh ki)
                Page<DocumentOutDto> docPage = null;
                DocumentStatusEnum[] docStatus = {DocumentStatusEnum.CHO_BAN_HANH, DocumentStatusEnum.DANG_XU_LY};
                docPage = docOutRepo.getListDocProcessAdvanceAssign(text, DocumentTypeEnum.VAN_BAN_DI, important,
                        u.getId(), numberOrSignL, previewL, orgCreateNameL, startCreateL, endCreateL, startIssuedL,
                        endIssuedL, docTypeL, docFieldsL, bookIdL, docStatus, u.getClientId(), isInternalDocument, page);
                return BussinessCommon.paging(castToDocIssuedDtoTrinhKi(docPage, page, docPage.getTotalElements()));

            }
            Boolean getFinishedInternalDoc = false;
            if (type != null && type.longValue() == 3) { // tab ban hanh - cho ban hanh
                codes[0] = DocumentStatusEnum.CHO_BAN_HANH;
            } else if (type != null && type == 5){ // tab vb noi bo - hoan thanh
                getFinishedInternalDoc = true;
                codes[0] = DocumentStatusEnum.DA_BAN_HANH;

            }else { // tab ban hanh - da ban hanh
                codes[0] = DocumentStatusEnum.DA_BAN_HANH;
            }

            if (!userService.isVanThuVBDi(u)) {
                return BussinessCommon.paging(null);
            }

            docOutPage = docOutRepo.getListDocAdvance(text, clericalOrg, DocumentTypeEnum.VAN_BAN_DI, important, null,
                    u.getId(), numberOrSignL, previewL, orgCreateNameL, startCreateL, endCreateL, startIssuedL,
                    endIssuedL, docTypeL, docFieldsL, bookIdL, codes, getFinishedInternalDoc, u.getClientId(), u.getOrg(), isInternalDocument, page);
        }

        return BussinessCommon.paging(castToDocIssuedDto(docOutPage, page, docOutPage.getTotalElements(), false));
    }

    public Page<KnowableDto> convertToPage(List<KnowableDto> dto, Boolean read, Pageable pageable) {
        if (Boolean.TRUE.equals(read)) {
            dto = dto.stream().filter(i -> Boolean.TRUE.equals(i.getRead())).collect(Collectors.toList());
        } else {
            dto = dto.stream().filter(i -> Boolean.FALSE.equals(i.getRead())).collect(Collectors.toList());
        }
        return BussinessCommon.getListToPage(dto, pageable);
    }

    public Page<KnowableDto> knowable(Boolean done, Pageable pageable) {
        List<Long> signerDocIds = signerDocumentOut();
        Page<KnowableDto> dto = docOutRepo.findKnowable(DocumentTypeEnum.VAN_BAN_DI, BussinessCommon.getUserId(),
                DocumentStatusEnum.DA_BAN_HANH, signerDocIds, done, pageable);
        fillAtt(dto.getContent());
        return dto;

    }

    public Page<KnowableDto> quickKnowable(String text, Boolean done, Pageable pageable) {
        List<Long> signerDocIds = signerDocumentOut();
        Page<KnowableDto> dto = docOutRepo.quickKnowable(DocumentTypeEnum.VAN_BAN_DI, text, BussinessCommon.getUser().getId(),
                DocumentStatusEnum.DA_BAN_HANH, signerDocIds, done, pageable);
        fillAtt(dto.getContent());
        return dto;
    }

    public Page<KnowableDto> searchKnowable(Boolean important, String preview, String numberOrSign, Long docTypeId,
                                            Long docFieldId, String orgCreateName, String userEnter, Date startDate, Date endDate, Date startIssued,
                                            Date endIssued, Boolean done, HandleTypeEnum handleType, Pageable pageable) {
        Long docFieldIdSigner = 0L;
        Long docTypeIdSigner = 0L;
        if (docFieldId != null) {
            docFieldIdSigner = docFieldId;
        }
        if (docTypeId != null) {
            docTypeIdSigner = docTypeId;
        }
        Date startDateTime = new Date();
        Date endDateTime = new Date();
        Date startIssuedTime = new Date();
        Date endIssuedTime = new Date();
        if (startDate != null) {
            startDateTime = startDate;
        }
        if (endDate != null) {
            endDateTime = endDate;
        }
        if (startIssued != null) {
            startIssuedTime = startIssued;
        }
        if (endIssued != null) {
            endIssuedTime = endIssued;
        }
        List<Long> signerDocIds = docOutRepo.signerDocumentOutSearch(BussinessCommon.getUser().getId(), BussinessCommon.getClientId(),
                preview, numberOrSign, orgCreateName, docFieldIdSigner, docTypeIdSigner, userEnter, startDate, endDate, startIssued, endIssued,
                startDateTime, endDateTime, startIssuedTime, endIssuedTime);
        Page<KnowableDto> dto = docOutRepo.searchKnowable(DocumentTypeEnum.VAN_BAN_DI, important,
                BussinessCommon.getClientId(), preview, numberOrSign, docTypeId, docFieldId,
                orgCreateName, userEnter, startDate, endDate, startIssued, endIssued, BussinessCommon.getUser().getId(),
                DocumentStatusEnum.DA_BAN_HANH, signerDocIds, handleType, done, pageable);

        fillAtt(dto.getContent());
        return dto;
    }

    private void fillAtt(List<KnowableDto> content) {
        Map<Long, List<DocumentOutAttachment>> mapAtt = new HashMap<>();
        List<Long> docIds = new ArrayList<>();
        for (KnowableDto dto : content) {
            Long docId = dto.getId();
            docIds.add(docId);
            mapAtt.put(docId, new ArrayList<>());
        }
        for (DocumentOutAttachment attachment : doaRepo.findAllByDocIdIn(docIds)) {
            Long docId = attachment.getDocId();
            mapAtt.get(docId).add(attachment);
        }

//		Map<Long, Boolean> objReadMap = objReadService.getObjReadMap(BussinessCommon.getUserId(), docIds, DocumentTypeEnum.VAN_BAN_DI);
        for (KnowableDto dto : content) {
            Long docId = dto.getId();
            dto.setAttachments(mapAtt.get(docId));
            dto.setCanForward(wasForwarded(docId, BussinessCommon.getUserId()));
            dto.setCanAddUser(canAddUser(docId, BussinessCommon.getUserId()));
//			// set read
//			dto.setRead(setRead(docId, objReadMap));
        }
    }

    public ListObjectDto<DocOutIssuedDto> getListAll(String text, String outsideReceive, Boolean important,
                                                     String numberOrSign, String orgCreateName, Long orgReceiveId, String personEnter, String preview, Date startCreate,
                                                     Date endCreate, Date startIssued, Date endIssued, Long docTypeId, Long docFieldId, Long bookId, DocumentStatusEnum docStatus,
                                                     Pageable page) {
        User u = BussinessCommon.getUser();
        boolean isLibrarian = userService.isVanThuVBDi(u);
        List<Long> signerDocIds = signerDocumentOut();
        Page<DocumentOut> doPage = docOutRepo.getListAllAdvance(new Date(), text, clericalOrg, outsideReceive,
                important, u.getOrg(), personEnter, u.getId(), isLibrarian, numberOrSign, preview,
                StringUtils.decodeFromUrl(orgCreateName), orgReceiveId, startCreate, endCreate, startIssued, endIssued, docTypeId,
                docFieldId, bookId, u.getClientId(), signerDocIds, docStatus, page);
        return BussinessCommon.paging(castToDocIssuedDto(doPage, page, doPage.getTotalElements(), true));
    }

    public ReportDto report() {
        ReportDto r = new ReportDto();
        List<DocumentInHandleStatusEnum> handleStatusDone = DocumentInHandleStatusEnum.getHandleStatusByName("DA_XU_LY",
                "DA_TRA_LAI");
        List<DocumentInHandleStatusEnum> handleStatusReturn = DocumentInHandleStatusEnum.getHandleStatusByName("DA_TRA_LAI");
        List<DocumentInHandleStatusEnum> handleStatusDoing = DocumentInHandleStatusEnum
                .getHandleStatusByName("DANG_XU_LY", "XIN_DANH_GIA", "DG_CHAP_NHAN", "DG_TU_CHOI");
        List<DocumentStatusEnum> docStatusDoing = Arrays.asList(DocumentStatusEnum.DOING,
                DocumentStatusEnum.RETURN_DOC);
        List<DocumentStatusEnum> docStatusDone = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC,
                DocumentStatusEnum.DONE);

        if (userService.isVanThuVBDen(BussinessCommon.getUser())) {
            handleStatusDoing = DocumentInHandleStatusEnum.getHandleStatusByName("DANG_XU_LY", "DA_XU_LY");
            docStatusDone = Arrays.asList(DocumentStatusEnum.DONE);
        }

        // for doc in
        ReportDocumentDto docIn = new ReportDocumentDto();
        docIn.setProcessed(prService.countDocByUser(handleStatusDoing, docStatusDoing));
        docIn.setDoneOrReceive(prService.countDocByUser(handleStatusDone, docStatusDone));
        docIn.setOverDue(prService.countDocByUserAndOverDue(handleStatusDone, docStatusDoing));
        docIn.setReturnDoc(prService.countDocByUser(handleStatusReturn, docStatusDone));
        ReportDocByTypeDto inReport = docInProcessService.reportDocByType();
        docIn.setNotYet(inReport.getDocInMain() + inReport.getDocInSupport() + inReport.getDocInKnow() + inReport.getDocInOpinion());
        docIn.setChoXLC(inReport.getDocInMain());
        docIn.setChoXLPH(inReport.getDocInSupport());
        docIn.setChoNDB(inReport.getDocInKnow());
        docIn.setChoCD(inReport.getDocInDirection());

        // for doc out
        ReportDocumentDto docOut = new ReportDocumentDto();
        DocumentOutHandleStatusEnum[] enums;
        ReportDocByTypeDto outReport = docOutProcessService.reportDocByType();
        docOut.setNotYet(outReport.getDraftHandle());

        enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.DA_XU_LY};
        docOut.setDoneOrReceive(docOutProcessService.countDocByUser(enums));

        enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.BI_TRA_LAI};
        docOut.setReturnDoc(docOutProcessService.countDocByUser(enums));

        r.setDocIn(docIn);
        r.setDocOut(docOut);
        return r;
    }

    private ProcessByMonth simpleMonth(SimpleProcess simple, HashMap<String, ProcessByMonth> mapMonth, int groupMonth) {
        ReportKey key = simple.getKey(groupMonth);
        if (!mapMonth.containsKey(key.getKey())) {
            mapMonth.put(key.getKey(), new ProcessByMonth(key));
        }
        return mapMonth.get(key.getKey());
    }

    public List<ProcessByMonth> fullReport(Date startDate, Date endDate, int groupMonth) {
        User u = BussinessCommon.getUser();
        HashMap<String, ProcessByMonth> mapMonth = new HashMap<>();
        List<SimpleProcessIn> processInList = dipRepo.fullReport(u.getId(), u.getClientId(), startDate, endDate);
        List<SimpleProcessOut> processOutList = dopRepo.fullReport(u.getId(), u.getClientId(), startDate, endDate);
        List<SimpleProcessTask> processTaskList = taskRepo.fullReport(u.getId(), u.getClientId(), startDate, endDate);

        processInList.forEach(simple -> {
            ProcessByMonth pb = simpleMonth(simple, mapMonth, groupMonth);
            pb.add(simple);
        });
        processOutList.forEach(simple -> {
            ProcessByMonth pb = simpleMonth(simple, mapMonth, groupMonth);
            pb.add(simple);
        });
        processTaskList.forEach(simple -> {
            ProcessByMonth pb = simpleMonth(simple, mapMonth, groupMonth);
            pb.add(simple);
        });

        Date minDate = startDate;
        Date maxDate = endDate;
        List<SimpleProcess> allProcess = new ArrayList<>();
        allProcess.addAll(processInList);
        allProcess.addAll(processOutList);
        allProcess.addAll(processTaskList);
        for (SimpleProcess simple : allProcess) {
            Date currDate = simple.getUpdateDate();
            if (minDate == null || minDate.compareTo(currDate) > 0) {
                minDate = currDate;
            }
            if (maxDate == null || maxDate.compareTo(currDate) < 0) {
                maxDate = currDate;
            }
        }

        if (minDate != null && maxDate != null) {
            Date endTime = DateTimeUtils.handleSubmit(maxDate, Calendar.MILLISECOND, -1);
            SimpleProcess tmp = new SimpleProcess(0L, 0L, minDate);
            Calendar cal = Calendar.getInstance(DateTimeUtils.timeZone());
            cal.setTime(minDate);
            cal.add(Calendar.MILLISECOND, 1);
            cal.set(Calendar.DAY_OF_MONTH, 1);
            Date now = cal.getTime();

            while (now.compareTo(endTime) < 0) {
                tmp.setUpdateDate(now);
                simpleMonth(tmp, mapMonth, groupMonth);
                cal.set(Calendar.MONTH, cal.get(Calendar.MONDAY) + groupMonth);
                now = cal.getTime();
            }
        }

        List<ProcessByMonth> result = new ArrayList<>(mapMonth.values());
        result.sort(Comparator.comparing(ProcessByMonth::getKey));
        return result;
    }

    public List<DocOutIssuedDto> exportExcel(String text, String outsideReceive, Boolean important, String numberOrSign, String orgCreateName,
                                             String personEnter, String preview, Date startCreate, Date endCreate, Date startIssued, Date endIssued,
                                             Long docTypeId, Long docFieldId, Long bookId, Long orgReceiveId, DocumentStatusEnum status) {
        List<DocumentOut> doPage = exportExcelToList(text, outsideReceive, important, numberOrSign, orgCreateName, personEnter, preview,
                startCreate, endCreate, startIssued, endIssued, docTypeId, docFieldId, bookId, status, orgReceiveId);
        return castToDocIssuedDtoByList(doPage, null, false);
    }

    public ObjectNode exportExcelByConfig(String text, String outsideReceive, Boolean important, String numberOrSign, String orgCreateName,
                                          String personEnter, String preview, Date startCreate, Date endCreate, Date startIssued, Date endIssued,
                                          Long docTypeId, Long docFieldId, Long bookId, Long orgReceiveId, DocumentStatusEnum status) {
        List<DocumentOut> rsList = exportExcelToList(text, outsideReceive, important, numberOrSign, orgCreateName, personEnter, preview,
                startCreate, endCreate, startIssued, endIssued, docTypeId, docFieldId, bookId, status, orgReceiveId);
        return rpFieldService.getData(DocumentTypeEnum.VAN_BAN_DI, rsList);
    }

    public List<FindDocDto> exportExcelOut(String numberOrSign,
                                           String preview, Date startCreate, Date endCreate,
                                           Long docTypeId, Long docFieldId, DocumentTypeEnum docType, DocumentStatusEnum status, Long bookId, Long urgentId, Long securityId, Long orgIssuedId) {
        User u = BussinessCommon.getUser();
        List<Long> orgIds = orgService.orgAndSub(u.getOrg());
        Long userVanThuId = userRepository.getUserVanThuBan(Constant.VAN_THU_MAIN, orgService.getRootOrgId(u.getOrg()));
        List<String> userLeadNames = new ArrayList<>();
        userLeadNames.add(Constant.POSITION_MAIN);
        userLeadNames.add(Constant.POSITION_SECOND);
        List<Long> listUser = clericalOrgRepository.getClericalOrgByOrgIdAndClientId(orgService.getRootOrgId(u.getOrg()), BussinessCommon.getClientId());
        for (Long userIdVTDV : listUser) {
            User userOrg = userService.findByUserId(userIdVTDV);
            if (userOrg != null) {
                orgIds.add(userOrg.getOrg());
            }
        }
        List<Long> userLanhDaoBanIds = userRepository.getUserLeadOrgBan(userLeadNames, orgService.getRootOrgId(u.getOrg()));
        List<DocumentOut> docList = docOutRepo.exportAllDoc(numberOrSign, preview, startCreate, endCreate, docTypeId, docFieldId, u.getClientId(), orgIds, status, bookId, urgentId, securityId, orgIssuedId);
        docList.addAll(docOutRepo.exportFollowDocumentVTBBanHanh(numberOrSign, preview, startCreate, endCreate, docFieldId, u.getClientId(), userVanThuId, DocumentTypeEnum.VAN_BAN_DI, DocumentStatusEnum.DA_BAN_HANH, bookId, urgentId, securityId, orgIssuedId, docTypeId));
        docList.addAll(docOutRepo.exportFollowDocumentLeadBan(numberOrSign, preview, startCreate, endCreate, docFieldId, u.getClientId(), userLanhDaoBanIds, DocumentTypeEnum.VAN_BAN_DI, DocumentStatusEnum.DONE, bookId, urgentId, securityId, orgIssuedId, docTypeId));

        int no = 0;
        List<FindDocDto> rsList = new ArrayList<>();
        for (DocumentOut d : docList) {
            FindDocDto rs = new FindDocDto(d);
            rs.setNo(++no);
            rsList.add(rs);
        }
        HashMap<String, Long> hashMap = new HashMap<>();
        List<Long> listLong = new ArrayList<>();
        for (int i = 0; i < rsList.size(); i++) {
            if (rsList.get(i).getParentId() != null) {
                if (!hashMap.containsKey(rsList.get(i).getParentId().toString())) {
                    hashMap.put(rsList.get(i).getParentId().toString(), rsList.get(i).getParentId());
                    listLong.add(rsList.get(i).getId());
                }
            } else {
                listLong.add(rsList.get(i).getId());
            }
        }

        List<Long> orgIdBan = orgService.orgAndSub(orgService.getRootOrgId(BussinessCommon.getOrgId()));
        Long userBanThuBan = userRepository.getUserVanThuBan(Constant.VAN_THU_MAIN, orgIdBan);
        List<DocumentOut> docResults = docOutRepo.findAllDocByIdsNotPaging(listLong, userBanThuBan);

        int no1 = 0;
        List<FindDocDto> objecList = new ArrayList<>();
        for (DocumentOut d : docResults) {
            // set list receive
            List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdTypeNotTransfer(BussinessCommon.getClientId(),
                    d.getId());
            FindDocDto rs = new FindDocDto(d);
            rs.setNo(++no1);
            rs.setListReceive(getDataDrList(receiveIdList, d.getId()));
            objecList.add(rs);
        }
        return objecList;
    }

    public List<DocumentOut> exportExcelToList(String text, String outsideReceive, Boolean important,
                                               String numberOrSign, String orgCreateName, String personEnter, String preview, Date startCreate,
                                               Date endCreate, Date startIssued, Date endIssued, Long docTypeId, Long docFieldId, Long bookId, DocumentStatusEnum status, Long orgReceiveId) {
        User u = BussinessCommon.getUser();
        boolean isLibrarian = userService.isVanThuVBDi(u);
        List<Long> signerDocIds = signerDocumentOut();
        return docOutRepo.getListAllAdvanceNotPaging(new Date(), text, clericalOrg, outsideReceive, important,
                u.getOrg(), personEnter, u.getId(), isLibrarian, numberOrSign, preview,
                StringUtils.decodeFromUrl(orgCreateName), startCreate, endCreate, startIssued, endIssued, docTypeId,
                docFieldId, bookId, u.getClientId(), signerDocIds, status, orgReceiveId);
    }

    public List<DocumentOut> exportExcelInToList(String numberOrSign, String preview, Date startCreate,
                                                 Date endCreate, Long docTypeId, Long docFieldId) {
        User u = BussinessCommon.getUser();
//		boolean isLibrarian = userService.isVanThuVBDi(u);
        List<Long> signerDocIds = signerDocumentOut();
        return docOutRepo.getListsAllAdvanceNotPaging(numberOrSign, preview, docFieldId, startCreate, endCreate, docTypeId, u.getClientId());
    }

    public Page<FindDocDto> findAll(FindDocDto dto, Integer page) {
        Sort sort = Sort.by(Direction.DESC, "updateDate", "createDate");
        User u = BussinessCommon.getUser();
        dto = dto.convert(dto);
        dto.setCreateFrom(DateTimeUtils.getStartDate(dto.getCreateFrom()));
        dto.setDateIssuedFrom(DateTimeUtils.getStartDate(dto.getDateIssuedFrom()));
        dto.setCreateTo(DateTimeUtils.getEndDate(dto.getCreateTo()));
        dto.setDateIssuedTo(DateTimeUtils.getEndDate(dto.getDateIssuedTo()));
        Page<DocumentOut> docList = docOutRepo.findAll(dto.getNumberOrSign(), dto.getPreview(), dto.getDocFieldsId(),
                dto.getDocStatusId(), dto.getUrgentId(), dto.getSecurityId(), u.getId(), dto.getOrgIssuedId(), dto.getCreateFrom(),
                dto.getCreateTo(), dto.getDateIssuedFrom(), dto.getDateIssuedTo(), u.getClientId(),
                BussinessCommon.castToPageable(page, sort));
        if (BussinessCommon.isEmptyPage(docList)) {
            return null;
        }
        int no = 0;
        List<FindDocDto> rsList = new ArrayList<>();
        for (DocumentOut d : docList) {
            FindDocDto rs = new FindDocDto(d);
            rs.setNo(++no);
            rsList.add(rs);
        }
        return new PageImpl<>(rsList, BussinessCommon.castToPageable(page), docList.getTotalElements());
    }

    public String getDocTypeName(Long docId) {
        return docOutRepo.getDocTypeName(docId);
    }

    public Page<FindDocDto> findAllDoc(FindDocDto dto, Integer page) {
//		Sort sort = Sort.by(Direction.ASC, "updateDate", "createDate");
//		Date startCreate = DateTimeUtils.handleSubmit(dto.getCreateFrom(), Calendar.MILLISECOND, -1);
//		Date endCreate = DateTimeUtils.handleSubmit(dto.getCreateTo(), Calendar.DAY_OF_MONTH, 1);
        dto = dto.convert(dto);
        dto.setCreateFrom(DateTimeUtils.getStartDate(dto.getCreateFrom()));
        dto.setCreateTo(DateTimeUtils.getEndDate(dto.getCreateTo()));
        User u = BussinessCommon.getUser();
        List<Long> orgIds = orgService.orgAndSub(u.getOrg());
        List<Long> userVanThuId = userRepository.getUsersByNameCategory(Constant.VAN_THU_MAIN, orgService.getRootOrgId(u.getOrg()));
        List<String> userLeadNames = new ArrayList<>();
        userLeadNames.add(Constant.POSITION_MAIN);
        userLeadNames.add(Constant.POSITION_SECOND);
        userLeadNames.add(Constant.CAN_BO_HANH_CHINH_BV);
        List<Long> listUser = clericalOrgRepository.getClericalOrgByOrgIdAndClientId(orgService.getRootOrgId(u.getOrg()), BussinessCommon.getClientId());
        for (Long userIdVTDV : listUser) {
            User userOrg = userService.findByUserId(userIdVTDV);
            if (userOrg != null) {
                orgIds.add(userOrg.getOrg());
            }
        }
        List<Long> userLanhDaoBanIds = userRepository.getUserLeadOrgBan(userLeadNames, orgService.getRootOrgId(u.getOrg()));
        List<DocumentOut> docList = docOutRepo.findAllDoc(dto.getNumberOrSign(), dto.getPreview(), dto.getDocFieldsId(),
                dto.getStatus(), dto.getUrgentId(), dto.getSecurityId(), dto.getOrgIssuedId(), dto.getCreateFrom(), dto.getCreateTo(), dto.getBookId(), u.getClientId(), orgIds, dto.getOrgReceiveId());
        docList.addAll(docOutRepo.getFollowDocumentVTBBanHanh(dto.getNumberOrSign(), dto.getBookId(), dto.getPreview(), dto.getDocFieldsId(),
                dto.getStatus(), dto.getUrgentId(), dto.getOrgIssuedId(), dto.getOrgReceiveId(), dto.getCreateFrom(), dto.getCreateTo(), dto.getSecurityId(), u.getClientId(), userVanThuId, DocumentTypeEnum.VAN_BAN_DI, DocumentStatusEnum.DA_BAN_HANH));
        docList.addAll(docOutRepo.getFollowDocumentLeadBan(dto.getNumberOrSign(), dto.getBookId(), dto.getPreview(), dto.getDocFieldsId(),
                dto.getStatus(), dto.getUrgentId(), dto.getOrgIssuedId(), dto.getOrgReceiveId(), dto.getCreateFrom(), dto.getCreateTo(), dto.getSecurityId(), u.getClientId(), userLanhDaoBanIds, DocumentTypeEnum.VAN_BAN_DI, DocumentStatusEnum.DONE));

        int no = 0;
        List<FindDocDto> rsList = new ArrayList<>();
        for (DocumentOut d : docList) {
            FindDocDto rs = new FindDocDto(d);
            rs.setNo(++no);
            rsList.add(rs);
        }
        HashMap<String, Long> hashMap = new HashMap<>();
        List<Long> listLong = new ArrayList<>();
        for (int i = 0; i < rsList.size(); i++) {
            if (rsList.get(i).getParentId() != null) {
                if (!hashMap.containsKey(rsList.get(i).getParentId().toString())) {
                    hashMap.put(rsList.get(i).getParentId().toString(), rsList.get(i).getParentId());
                    listLong.add(rsList.get(i).getId());
                }
            } else {
                listLong.add(rsList.get(i).getId());
            }
        }
        Pageable pageable = BussinessCommon.castToPageable(page, dto.getPageSize());
        List<Long> orgIdBan = orgService.orgAndSub(orgService.getRootOrgId(BussinessCommon.getOrgId()));
        List<Long> userBanThuBan = userRepository.getUsersByNameCategory(Constant.VAN_THU_MAIN, orgIdBan);
        Page<DocumentOut> docResults = docOutRepo.findAllDocByIds(listLong, userBanThuBan, pageable);
        if (BussinessCommon.isEmptyPage(docResults)) {
            return null;
        }
        int no1 = 0;
        List<FindDocDto> objecList = new ArrayList<>();
        for (DocumentOut d : docResults.getContent()) {
            // set list receive
            List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdTypeNotTransfer(BussinessCommon.getClientId(),
                    d.getId());
            FindDocDto rs = new FindDocDto(d);
            rs.setNo(++no1);
            rs.setListReceive(getDataDrList(receiveIdList, d.getId()));
            objecList.add(rs);
        }
        return new PageImpl<>(objecList, pageable, docResults.getTotalElements());
    }

    public boolean importDocBook(Long docId, Long bookId, Long numberInBook, String numberOrSign) {
        DocumentOut doc = findByDocId(docId);
        if (!Boolean.TRUE.equals(bpmnService.getImportDocBookByNodeId(doc.getNodeId()))) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        if (doc.getNumberInBook() != null) {
            throw new RestExceptionHandler("Văn bản này đã được vào sổ");
        }
        DocumentBook docBook = docBookService.findByBookId(bookId);
        if ((doc.getNumberInBook() != null) || (numberInBook != docBook.getCurrentNumber() + 1)) {
            throw new RestExceptionHandler(Message.NUMBER_IN_BOOK_EXIST);
        }
        doc.setBookId(bookId);
        doc.setNumberInBook(numberInBook);
        doc.setNumberOrSign(numberOrSign);
        save(doc);
        docBook.setCurrentNumber(numberInBook);
        docBookService.save(docBook);
        return true;
    }

    public ActionControlDto checkAction(Long docId, Tab tab, Boolean isDelegate) {
        List<DocumentOutHandleStatusEnum> rejectStatus = Arrays.asList(DocumentOutHandleStatusEnum.CHO_XU_LY,
                DocumentOutHandleStatusEnum.BI_TRA_LAI);
        List<DocumentOutHandleStatusEnum> finishStatus = Arrays.asList(DocumentOutHandleStatusEnum.DU_THAO,
                DocumentOutHandleStatusEnum.CHO_XU_LY, DocumentOutHandleStatusEnum.BI_TRA_LAI);

        ActionControlDto result = new ActionControlDto(docId);
        if (Tab.DA_XU_LY.equals(tab)) {
            result.setCanRetake(checkCanRetake(docId, isDelegate));
        }
        if (Tab.CHO_XU_LY.equals(tab)) {
            result.setImportDocBook(checkImportDocBook(docId));
        }
        Boolean canFinish = null;
        Boolean canReject = null;
        Boolean canConsult = null;
        List<DocumentOutProcess> pList = docOutProcessService.findByUserRelatedAndDocId(docId, getUserOrListVanThuBan());
        for (DocumentOutProcess i : pList) {
            if (finishStatus.contains(i.getHandleStatus())) {
                canFinish = true;
                canConsult = true;
            }

            if (rejectStatus.contains(i.getHandleStatus())) {
                canReject = true;
            }
        }

        result.setCanFinish(canFinish);
        result.setCanReject(canReject);
        result.setCanConsult(canConsult);
        return result;
    }

    public Boolean checkCanRetake(Long docId, Boolean isDelegate) {
        Long currUser = BussinessCommon.getUserId();
        // Check process status
        DocumentOutHandleStatusEnum[] enums = {DocumentOutHandleStatusEnum.CHO_XU_LY,
                DocumentOutHandleStatusEnum.BI_TRA_LAI};
        DocumentOutProcess input = docOutProcessService.findFirstByActiveAndDocIdAndHandleStatusInOrderByIdDesc(true, docId, enums);
        if (input == null) {
            return false;
        }
        Long toUserId = input.getUserId();
        DocumentOutTracking dot = docOutTrackingService.getLastTrackingByDocIdAndToUserIdAndAction(docId, toUserId, DocumentOutTrackingEnum.TRANSFER);
        if (dot == null) {
            return false;
        }
        if (currUser.equals(dot.getFromUserId())) {
            return true;
        }
        if (Boolean.TRUE.equals(isDelegate)) {
            enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.DA_XU_LY};
            DocumentOutProcess preUserProcess = docOutProcessService.findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(true, docId, dot.getFromUserId(), enums);
            if (preUserProcess == null) {
                return false;
            }
            if (currUser.equals(preUserProcess.getDelegate().getToUserId())) {
                return true;
            }
        }
        return false;
    }

    private void validateImportDocBook(DocumentOut doc) {
        if (checkImportDocBook(doc)) {
            throw new RestExceptionHandler("Yêu cầu vào sổ văn bản trước khi Chuyển xử lý/Hoàn thành văn bản");
        }
    }

    private boolean checkImportDocBook(DocumentOut doc) {
        Boolean importDB = bpmnService.getImportDocBookByNodeId(doc.getNodeId());
        return (Boolean.TRUE.equals(importDB) && doc.getNumberInBook() == null);
    }

    private boolean checkImportDocBook(Long docId) {
        return docOutRepo.checkImportDocBookByDocId(docId);
    }

    public List<DocumentOut> getByRemind() {
        User u = BussinessCommon.getUser();
        boolean isLibrarian = userService.isVanThuVBDi(u);
        DocumentStatusEnum[] docStatusId = {DocumentStatusEnum.THU_HOI_BH};
        return docOutRepo.findByUserIdAndClientId(clericalOrg, docStatusId,
                u.isLead(), u.getOrg(), Constant.RECEIVER_TYPE_ORG, Constant.RECEIVER_TYPE_USER,
                u.getId(), isLibrarian, DocumentStatusEnum.CHO_BAN_HANH,
                DocumentStatusEnum.DA_BAN_HANH, u.getClientId());
    }

    public List<DocumentOut> findByIds(List<Long> ids) {
        return docOutRepo.findByClientIdAndActiveTrueAndIdIn(BussinessCommon.getClientId(), ids);
    }

    public boolean checkNumberOrSign(String numberOrSign, Long bookId) {
        if (org.apache.commons.lang3.StringUtils.isBlank(numberOrSign)) {
            return false;
        }
        return docOutRepo.checkNumberOrSign(DocumentOut.normalizeSign(numberOrSign), bookId, BussinessCommon.getClientId(), DocumentStatusEnum.THU_HOI_BH);
    }

    public boolean addSignUser(Long docId, Long userId) {
        // save doc
        DocumentOut data = findByDocId(docId);
        data.addSignUser(userId);
        save(data);
        return true;
    }

    public List<DocumentOutProcessDto> nodeReject(Long docId) {
        DocumentOutHandleStatusEnum[] enums = {
                DocumentOutHandleStatusEnum.DA_TRINH_KY, DocumentOutHandleStatusEnum.DA_XU_LY};
        List<Long> userTransfers = docOutTrackingService.findListUserTransfer(docId);
        return dopRepo.findListUserTransfer(docId, BussinessCommon.getClientId(), userTransfers, enums);
    }

    @Transactional
    public Boolean rejectByNodeId(Long docId, Long userId, Long nodeId, String comment, Boolean delegate, MultipartFile[] files) {
        Long currUser = BussinessCommon.getUserId();
        docOutCommentService.validCommentLength(comment);
        // Check process status
        DocumentOutHandleStatusEnum[] enums = {DocumentOutHandleStatusEnum.CHO_XU_LY,
                DocumentOutHandleStatusEnum.BI_TRA_LAI};
        //		DocumentOutProcess oldProcess = docOutProcessService.findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(true,
        //				docId, currUser, enums);
        DocumentOutProcess oldProcess = docOutProcessService.findByUserIdOrDelegateId(docId, getUserOrListVanThuBan(), enums);
        if (oldProcess == null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }

        enums = new DocumentOutHandleStatusEnum[]{DocumentOutHandleStatusEnum.DA_XU_LY,
                DocumentOutHandleStatusEnum.DA_TRINH_KY};
        DocumentOutProcess preProcess = docOutProcessService
                .findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(true, docId, userId,
                        enums);

        if (preProcess == null) {
            throw new RestExceptionHandler(Message.INVALID_PROCESS);
        }

        //		boolean delegate = false;
        //
        //		Long fromUserId = input.getUserId();
        //		if (!currUser.equals(fromUserId)) {
        //			delegate = true;
        //			// Check ủy quyền
        //			delegateService.checkDelegate(fromUserId, currUser);
        //		}

        DocumentOut doc = findByDocId(docId);
        try {
            // Update status and nodeId
            doc.setStatus(DocumentStatusEnum.BI_TRA_LAI);
            doc.setNodeId(nodeId);
            docOutRepo.save(doc);

            // update old process
            if (Boolean.TRUE.equals(delegate)) {
                docOutProcessService.saveOrUpdate(docId, currUser, currUser, oldProcess.getUserId(), null,
                        DocumentOutHandleStatusEnum.DA_TRA_LAI_UQ, null);
            }
            oldProcess.setHandleStatus(DocumentOutHandleStatusEnum.DA_TRA_LAI);
            docOutProcessService.save(oldProcess);
            //			if (delegate) {
            //				// Add process for currentUser
            //				docOutProcessService.save(docId, currUser, currUser, fromUserId,
            //						DocumentOutHandleStatusEnum.DA_TRA_LAI_UQ);
            //			}
            if (doc.getCreateBy().equals(userId)) {
                preProcess.setHandleStatus(DocumentOutHandleStatusEnum.DU_THAO);
                notiService.add(userId, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                        NotificationHandleStatusEnum.BI_TRA_LAI, ModuleCodeEnum.DRAFT_LIST);
            } else {
                preProcess.setHandleStatus(DocumentOutHandleStatusEnum.BI_TRA_LAI);
                notiService.add(userId, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                        NotificationHandleStatusEnum.BI_TRA_LAI, ModuleCodeEnum.DRAFT_HANDLE);
            }
            preProcess.setRead(false);
            docOutProcessService.save(preProcess);
            // Thêm comment và tracking
            if (!StringUtils.isNullOrEmpty(comment)) {
                DocumentOutComment cmt = docOutCommentService.saveCmt(docId, comment);
                docOutAttachService.addListAttachment(files, "comment", cmt.getId());
                docOutTrackingService.save(docId, currUser, currUser, DocumentOutTrackingEnum.REJECT,
                        cmt.getId());
            } else {
                docOutTrackingService.save(docId, currUser, currUser, DocumentOutTrackingEnum.REJECT, null);
            }
            // Xóa tất cả thông báo trước đó
            if (oldProcess.getDelegateId() != null) {
                notiService.setActiveByUserIdAndDocIdAndDocType(oldProcess.getDelegate().getToUserId(), docId,
                        DocumentTypeEnum.VAN_BAN_DI, false);
            }

            // check if exist delegate -> inactive process delegate
            if (preProcess.getDelegateId() != null) {
                notiService.add(preProcess.getDelegate().getToUserId(), docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                        NotificationHandleStatusEnum.BI_TRA_LAI_UQ, ModuleCodeEnum.DRAFT_HANDLE);
            }

        } catch (Exception e) {
            e.printStackTrace();
        }
        return true;
    }

    public List<String> getOutsideReceiveList() {
        return docOutRepo.getOutsideReceiveList(BussinessCommon.getClientId());
    }

    /**
     * Văn bản mà người hiện tại là người kí
     *
     * @return
     */
    public List<Long> signerDocumentOut() {
        List<KnowableDto> rs = docOutRepo.signerDocumentOut(BussinessCommon.getUserId(), BussinessCommon.getClientId());
        return rs.stream().map(KnowableDto::getId).collect(Collectors.toList());
    }

    @Autowired
    private IObjectReadRepository objectReadRepo;

    private DocumentReceive findExistDocReceive(Long userId, Long docId, String type, HandleTypeEnum handleType) {
        DocumentReceive dr = docReceiveRepo.findByClientIdAndDocIdAndReceiveIdAndTypeAndActiveTrue(BussinessCommon.getClientId(), docId, userId,
                type);
        if (dr != null) {
            return dr;
        } else {
            return new DocumentReceive(userId, docId, type, handleType);
        }

    }

    /**
     * Chuyển tiếp văn bản đi
     *
     * @param docId
     * @param main
     * @param support
     * @param show
     * @return
     */
    public DocumentOut forwardDoc(Long docId, Long main, List<Long> support, List<Long> show) {
        if (main == null && BussinessCommon.isEmptyList(support) && BussinessCommon.isEmptyList(show)) {
            throw new RestExceptionHandler(Message.INVALID_INPUT_DATA);
        }
        DocumentOut doc = valid(docId, Message.DOCUMENT_NOT_FOUND);

        List<DocumentReceive> docReceivers = new ArrayList<>();
        List<DocumentOutTracking> docTracking = new ArrayList<>();

        DocumentOutTracking tracking = new DocumentOutTracking(docId, BussinessCommon.getUserId(), null,
                DocumentOutTrackingEnum.FORWARD, BussinessCommon.getUser().getOrgModel().getName(), null, null);
        docTracking.add(tracking);

        if (main != null) {

            docReceivers.add(findExistDocReceive(main, docId, Constant.RECEIVER_TYPE_FORWARD, HandleTypeEnum.MAIN));

            tracking = new DocumentOutTracking(docId, BussinessCommon.getUserId(), null,
                    DocumentOutTrackingEnum.INCOMING, BussinessCommon.getUser().getOrgModel().getName(), main, null);
            docTracking.add(tracking);

        }

        if (!BussinessCommon.isEmptyList(support)) {
            for (Long userId : support) {

                docReceivers
                        .add(findExistDocReceive(userId, docId, Constant.RECEIVER_TYPE_FORWARD, HandleTypeEnum.SUPPORT));

                tracking = new DocumentOutTracking(docId, BussinessCommon.getUserId(), null,
                        DocumentOutTrackingEnum.INCOMING, BussinessCommon.getUser().getOrgModel().getName(), userId,
                        null);
                docTracking.add(tracking);

            }
        }

        if (!BussinessCommon.isEmptyList(show)) {
            for (Long userId : show) {

                docReceivers
                        .add(findExistDocReceive(userId, docId, Constant.RECEIVER_TYPE_FORWARD, HandleTypeEnum.SHOW));

                tracking = new DocumentOutTracking(docId, BussinessCommon.getUserId(), null,
                        DocumentOutTrackingEnum.INCOMING, BussinessCommon.getUser().getOrgModel().getName(), userId,
                        null);
                docTracking.add(tracking);

            }
        }

        docReceiveRepo.saveAll(docReceivers);
        docOutTrackingRepo.saveAll(docTracking);

        if (main != null) {
            notiService.add(main, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                    NotificationHandleStatusEnum.XU_LY_CHINH, ModuleCodeEnum.DOCUMENT_IN_LIST);
        }

        if (!BussinessCommon.isEmptyList(support)) {
            notiService.addAll(support, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                    NotificationHandleStatusEnum.PHOI_HOP, ModuleCodeEnum.DOCUMENT_IN_LIST);
        }

        if (!BussinessCommon.isEmptyList(show)) {
            notiService.addAll(show, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                    NotificationHandleStatusEnum.NHAN_DE_BIET, ModuleCodeEnum.DOCUMENT_IN_LIST);
        }

        docReceiveService.setStatus(docId, BussinessCommon.getUserId(), DocumentStatusEnum.DONE);
        return doc;
    }

    public List<UserConditionDto> getUserForwardList() {
        List<UserConditionDto> rs = new ArrayList<>();
        rs.addAll(docOutRepo.findListUserByOrgId(BussinessCommon.getClientId(), BussinessCommon.getOrgId(),
                BussinessCommon.getUserId()));
        if (Constant.BAN.equals(BussinessCommon.getUser().getOrgModel().getOrgTypeModel().getName())) {
            rs.addAll(docOutRepo.findByClientIdAndParentIdAndAuthority(BussinessCommon.getClientId(),
                    BussinessCommon.getUserId(), BussinessCommon.getOrgId(), AuthorityEnum.LEADERSHIP));
        }

        if (Constant.CUC_VU_VIEN.equals(BussinessCommon.getUser().getOrgModel().getOrgTypeModel().getName())) {
            rs.addAll(docOutRepo.findByClientIdAndParentIdAndAuthority(BussinessCommon.getClientId(),
                    BussinessCommon.getUserId(), BussinessCommon.getOrgId(), AuthorityEnum.LEADERSHIP_UNIT));
        }
        return rs;
    }

    public List<DocumentReceive> getReceivedUser(Long docId) {
        return docReceiveRepo.findByClientIdAndTypeAndDocId(BussinessCommon.getClientId(),
                Constant.RECEIVER_TYPE_FORWARD, docId);
    }

    /**
     * Button Hoàn thành xử lý
     *
     * @param docId
     * @param userId
     * @param comment
     * @return
     */
    public Boolean finishRead(Long docId, Long userId, String comment) {
        docReceiveService.setStatus(docId, userId, DocumentStatusEnum.DONE);
        if (!StringUtils.isNullOrEmpty(comment)) {
            DocumentOutComment cmt = docOutCommentService.saveCmt(docId, comment);
            docOutTrackingService.save(docId, DocumentOutTrackingEnum.HOAN_THANH, cmt.getId());
        }
        return true;
    }

    /**
     * Button Thu hồi hoàn thành
     *
     * @param docId
     * @param userId
     * @param comment
     * @return
     */
    public Boolean unfinishRead(Long docId, Long userId, String comment) {
        docReceiveService.setStatus(docId, userId, DocumentStatusEnum.NOT_YET);
        if (!StringUtils.isNullOrEmpty(comment)) {
            DocumentOutComment cmt = docOutCommentService.saveCmt(docId, comment);
            docOutTrackingService.save(docId, DocumentOutTrackingEnum.THU_HOI_HOAN_THANH, cmt.getId());
        }
        return true;
    }

    /**
     * Button Bổ sung xử lý
     *
     * @param docId
     * @param main
     * @param support
     * @param show
     * @param comment
     * @return
     */
    public DocumentOut forwarAdditional(Long docId, Long main, List<Long> support, List<Long> show, String comment) {
        if (main == null && BussinessCommon.isEmptyList(support) && BussinessCommon.isEmptyList(show)) {
            throw new RestExceptionHandler(Message.INVALID_INPUT_DATA);
        }
        DocumentOut doc = valid(docId, Message.DOCUMENT_NOT_FOUND);

        List<DocumentReceive> docReceivers = new ArrayList<>();
        List<DocumentOutTracking> docTracking = new ArrayList<>();
        DocumentOutTracking tracking = new DocumentOutTracking();
        if (!StringUtils.isNullOrEmpty(comment)) {
            DocumentOutComment cmt = docOutCommentService.saveCmt(docId, comment);
            docOutTrackingService.save(docId, DocumentOutTrackingEnum.FORWARD, cmt.getId());
        } else {
            tracking = new DocumentOutTracking(docId, BussinessCommon.getUserId(), null,
                    DocumentOutTrackingEnum.FORWARD, BussinessCommon.getUser().getOrgModel().getName(), null, null);
            docTracking.add(tracking);
        }

        if (main != null) {

            docReceivers.add(findExistDocReceive(main, docId, Constant.RECEIVER_TYPE_FORWARD, HandleTypeEnum.MAIN));

            tracking = new DocumentOutTracking(docId, BussinessCommon.getUserId(), null,
                    DocumentOutTrackingEnum.INCOMING, BussinessCommon.getUser().getOrgModel().getName(), main, null);
            docTracking.add(tracking);

        }

        if (!BussinessCommon.isEmptyList(support)) {
            for (Long userId : support) {

                docReceivers
                        .add(findExistDocReceive(userId, docId, Constant.RECEIVER_TYPE_FORWARD, HandleTypeEnum.SUPPORT));

                tracking = new DocumentOutTracking(docId, BussinessCommon.getUserId(), null,
                        DocumentOutTrackingEnum.INCOMING, BussinessCommon.getUser().getOrgModel().getName(), userId,
                        null);
                docTracking.add(tracking);

            }
        }

        if (!BussinessCommon.isEmptyList(show)) {
            for (Long userId : show) {

                docReceivers
                        .add(findExistDocReceive(userId, docId, Constant.RECEIVER_TYPE_FORWARD, HandleTypeEnum.SHOW));

                tracking = new DocumentOutTracking(docId, BussinessCommon.getUserId(), null,
                        DocumentOutTrackingEnum.INCOMING, BussinessCommon.getUser().getOrgModel().getName(), userId,
                        null);
                docTracking.add(tracking);

            }
        }

        docReceiveRepo.saveAll(docReceivers);
        docOutTrackingRepo.saveAll(docTracking);

        if (main != null) {
            notiService.add(main, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                    NotificationHandleStatusEnum.XU_LY_CHINH, ModuleCodeEnum.DOCUMENT_IN_LIST);
        }

        if (!BussinessCommon.isEmptyList(support)) {
            notiService.addAll(support, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                    NotificationHandleStatusEnum.PHOI_HOP, ModuleCodeEnum.DOCUMENT_IN_LIST);
        }

        if (!BussinessCommon.isEmptyList(show)) {
            notiService.addAll(show, docId, doc.getPreview(), DocumentTypeEnum.VAN_BAN_DI,
                    NotificationHandleStatusEnum.NHAN_DE_BIET, ModuleCodeEnum.DOCUMENT_IN_LIST);
        }

        docReceiveService.setStatus(docId, BussinessCommon.getUserId(), DocumentStatusEnum.DONE);
        return doc;
    }

    private Boolean wasForwarded(Long docId, Long userId) {
        List<DocumentOutTracking> track = docOutTrackingRepo.findByDocIdAndFromUserIdAndClientIdAndAndAction(docId,
                userId, BussinessCommon.getClientId(), DocumentOutTrackingEnum.FORWARD);

        List<DocumentReceive> receiveIdList = drService.findByClientIdAndDocIdAndType(BussinessCommon.getClientId(), docId, null);

        List<Long> userIds = getUserIdReceiveByDocId(receiveIdList, false);

        if (!BussinessCommon.isEmptyList(track) || !userIds.contains(userId)) {
            return false;
        } else {
            return true;
        }
    }

    private Boolean canAddUser(Long docId, Long userId) {
        String[] types = {"ORG", "USER", "ALL", "FORWARD"};
        List<DocumentOutTracking> track = docOutTrackingRepo.findByDocIdAndFromUserIdAndClientIdAndAndAction(docId,
                userId, BussinessCommon.getClientId(), DocumentOutTrackingEnum.FORWARD);

        List<DocumentReceive> receive = docReceiveRepo.findByClientIdAndDocIdAndReceiveIdAndTypeInAndActiveTrue(
                BussinessCommon.getClientId(), docId, userId, types);
        if (BussinessCommon.isEmptyList(track) || BussinessCommon.isEmptyList(receive)) {
            return false;
        } else {
            return true;
        }
    }

    /**
     * Lấy danh sách người kí văn bản đi với điều kiện người hiện tại là người tạo
     *
     * @return
     */
    public List<SignerDto> getSignerIdByCreator(String text) {
        List<SignerDto> signers = docOutRepo.getSignerIdByCreator(text, BussinessCommon.getUserId(),
                BussinessCommon.getClientId());
        List<SignerDto> rs = new ArrayList<>();
        Map<String, SignerDto> map = new HashMap<>();
        signers.forEach(i -> map.put(i.getFullName(), i));
        List<String> fullNames = signers.stream().map(SignerDto::getFullName).distinct().collect(Collectors.toList());
        for (String k : fullNames) {
            if (map.containsKey(k)) {
                SignerDto v = map.get(k);
                rs.add(new SignerDto(v.getId(), k, v.getOrgName(), v.getPosition(), v.getPhone()));
            }
        }
        return rs;
    }

    public void beforePrintDocIssuedData(Long docId, String numberOrSign, Date issuedDate) {
        DocIssuedData dto = new DocIssuedData(numberOrSign, issuedDate == null ? new Date() : issuedDate);
        if (!dto.valids())
            return;
        List<DocumentOutAttachment> atmList = docOutAttachService.getListAttachment(docId);
        String[] docExtArr = {"doc", "docx"};
        for (DocumentOutAttachment a : atmList) {
            String nameExt = FilenameUtils.getExtension(a.getDisplayName());
            if (nameExt == null || !Arrays.asList(docExtArr).contains(nameExt.toLowerCase())) {
                continue;
            }
            fileStorageService.printDocIssuedData(dto, a.getName());
        }
    }

    private List<Long> getUserOrListVanThuBan() {
        User user = BussinessCommon.getUser();
        Category positionVanThuBan = catService.findByName(user.getClientId(), Constant.VAN_THU_MAIN);
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

    public DocumentOut importDocBookAndIssued(Long docId, Long bookId, Long numberInBook, String numberOrSign) {
        DocumentOut doc = findByDocId(docId);
        if (!Boolean.TRUE.equals(bpmnService.getImportDocBookByNodeId(doc.getNodeId()))) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }
        if (doc.getNumberInBook() != null) {
            throw new RestExceptionHandler("Văn bản này đã được vào sổ");
        }
        DocumentBook docBook = docBookService.findByBookId(bookId);
        if ((doc.getNumberInBook() != null) || (numberInBook != docBook.getCurrentNumber() + 1)) {
            throw new RestExceptionHandler(Message.NUMBER_IN_BOOK_EXIST);
        }
        doc.setBookId(bookId);
        doc.setNumberInBook(numberInBook);
        doc.setNumberOrSign(numberOrSign);
        save(doc);
        docBook.setCurrentNumber(numberInBook);
        finish(docId, null, "");
        return doc;
    }
}
