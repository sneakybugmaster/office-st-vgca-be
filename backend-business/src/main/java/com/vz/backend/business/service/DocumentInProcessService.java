package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.vz.backend.core.config.*;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.repository.IUserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.config.DocumentCommentTypeEnum;
import com.vz.backend.business.controller.DocumentOutProcessController;
import com.vz.backend.business.domain.BpmnModel2.TYPE_DOCUMENT;
import com.vz.backend.business.domain.DocumentComment;
import com.vz.backend.business.domain.DocumentInProcess;
import com.vz.backend.business.domain.DocumentInTracking;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.dto.DocumentInConditionDto;
import com.vz.backend.business.dto.DocumentOutProcessDto;
import com.vz.backend.business.dto.HandlerDto;
import com.vz.backend.business.dto.NodeDto;
import com.vz.backend.business.dto.PDocDto;
import com.vz.backend.business.dto.ProcessDocumentDto;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.business.dto.TrackingDto;
import com.vz.backend.business.dto.TrackingReceiveDocDto;
import com.vz.backend.business.dto.TrackingSendReceiveDto;
import com.vz.backend.business.dto.UserConditionDto;
import com.vz.backend.business.dto.kpi.KPIDataDto;
import com.vz.backend.business.repository.IDelegateRepository;
import com.vz.backend.business.repository.IDocumentCommentRepository;
import com.vz.backend.business.repository.IDocumentInProcessRepository;
import com.vz.backend.business.repository.IDocumentRepository;
import com.vz.backend.business.repository.INodeRepository2;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.UserRoleService;
import com.vz.backend.core.service.UserService;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;

@Service
public class DocumentInProcessService extends BaseService<DocumentInProcess> {
    @Value("${configs.doc-in.internal-remove: false}")
    private boolean docInternalRemove;

    @Value("${configs.doc-in.review-required: false}")
    private boolean reviewRequired;

    @Value("${configs.doc-in.read-to-doing: false}")
    private boolean readToDoing;

    @Value("${configs.clerical-org: false}")
    private boolean clericalOrg;

    @Value("${configs.doc-in.main-require: false}")
    private boolean mainRequire;

    @Value("${configs.previous-last-node-show-button-done: false}")
    private boolean isPreviousLastNode;

    @Autowired
    IDelegateRepository delegateRepository;

    @Autowired
    IDocumentInProcessRepository processRepository;

    @Autowired
    UserService uService;

    @Autowired
    OrganizationService orgService;

    @Autowired
    IDocumentRepository docRepository;

    @Autowired
    DocumentInTrackingService trackingService;

    @Autowired
    AttachmentCommentService attachCmtService;

    @Autowired
    public DocumentCommentService cmtService;

    @Autowired
    CategoryService categoryService;

    @Autowired
    UserRoleService uRoleService;

    @Autowired
    NotificationService noticeService;

    @Autowired
    DelegateService delegateService;

    @Autowired
    private BpmnService2 bpmnService;

    @Autowired
    private DocumentInManipulationService manipulationService;

    @Autowired
    INodeRepository2 nodeRepo;

    @Autowired
    private IDocumentCommentRepository commentRepository;

    @Autowired
    private IUserRepository userRepository;

    @Override
    public IRepository<DocumentInProcess> getRepository() {
        return processRepository;
    }

    private static DocumentInHandleStatusEnum[] HANDLE_STATUS_DOING = {DocumentInHandleStatusEnum.CHO_XU_LY,
            DocumentInHandleStatusEnum.DANG_XU_LY, DocumentInHandleStatusEnum.CHO_CHO_Y_KIEN,
            DocumentInHandleStatusEnum.CHO_DANH_GIA, DocumentInHandleStatusEnum.XIN_DANH_GIA};

    public static DocumentInHandleStatusEnum[] DO_NOT_RELATED = {DocumentInHandleStatusEnum.DA_XU_LY_SWITCH,
            DocumentInHandleStatusEnum.DA_TRA_LAI};

    public static DocumentInHandleStatusEnum[] HANDLE_STATUS_NOT_YET = {DocumentInHandleStatusEnum.CHO_XU_LY,
            DocumentInHandleStatusEnum.DANG_XU_LY, DocumentInHandleStatusEnum.CHO_DANH_GIA,
//			DocumentInHandleStatusEnum.XIN_DANH_GIA, 
            DocumentInHandleStatusEnum.DG_CHAP_NHAN,
            DocumentInHandleStatusEnum.DA_XU_LY_ADD_USER, DocumentInHandleStatusEnum.THU_HOI_HOAN_THANH, DocumentInHandleStatusEnum.MOI_NHAN};

    public static DocumentInHandleStatusEnum[] HANDLE_STATUS_RETURN = {DocumentInHandleStatusEnum.CHO_XU_LY,
            DocumentInHandleStatusEnum.DANG_XU_LY, DocumentInHandleStatusEnum.CHO_DANH_GIA,
            DocumentInHandleStatusEnum.XIN_DANH_GIA, DocumentInHandleStatusEnum.DG_CHAP_NHAN,
            DocumentInHandleStatusEnum.DG_TU_CHOI, DocumentInHandleStatusEnum.DA_XU_LY_ADD_USER,
            DocumentInHandleStatusEnum.THU_HOI_HOAN_THANH};

    public static DocumentInHandleStatusEnum[] CHUYEN_DON_VI = {DocumentInHandleStatusEnum.CHUYEN_DON_VI_UQ,
            DocumentInHandleStatusEnum.CHUYEN_DON_VI};

    public static DocumentStatusEnum[] DOC_STATUS_NOT_TRANSFER = {DocumentStatusEnum.DONE,
            DocumentStatusEnum.RETAKE_DOC};

    public static DocumentInHandleStatusEnum[] DONE = {DocumentInHandleStatusEnum.DA_XU_LY,
            DocumentInHandleStatusEnum.DA_XU_LY_UQ};

    public static DocumentInHandleStatusEnum[] NOT_IN_DONE = {DocumentInHandleStatusEnum.DA_XU_LY_SWITCH,
            DocumentInHandleStatusEnum.DA_THU_HOI, DocumentInHandleStatusEnum.DA_XU_LY,
            DocumentInHandleStatusEnum.DA_TRA_LAI, DocumentInHandleStatusEnum.DA_TRA_LAI_UQ,
            DocumentInHandleStatusEnum.DA_XU_LY_UQ};

    public static List<DocumentInHandleStatusEnum> HANDLE_STATUS_DONE;

    static {
        HANDLE_STATUS_DONE = new ArrayList<>();
        Collections.addAll(HANDLE_STATUS_DONE, DO_NOT_RELATED);
        Collections.addAll(HANDLE_STATUS_DONE, DONE);
        Collections.addAll(HANDLE_STATUS_DONE, HANDLE_STATUS_NOT_YET);
    }

    public static List<DocumentInHandleStatusEnum> NOT_YET_ALL;

    static {
        NOT_YET_ALL = new ArrayList<>();
        Collections.addAll(NOT_YET_ALL, CHUYEN_DON_VI);
        Collections.addAll(NOT_YET_ALL, HANDLE_STATUS_NOT_YET);
    }

    public DocumentInProcess save(Long node, Long docId, DocumentInHandleStatusEnum handleStatus,
                                  HandleTypeEnum handleType, Integer step, Integer transferStep, User toUsers, Long frParam,
                                  Long assignHandle, Date deadline, Long delegateId) {
        User frUser = BussinessCommon.getUser();
        Organization org = orgService.validOrgId(toUsers.getOrg());
        DocumentInProcess o = findByDocIdAndStepAndHandleTypeAndToUser(docId, step, toUsers.getId(), handleType);
        DocumentInProcess p = o == null ? new DocumentInProcess() : o;
        p.setNode(node);
        p.setPreNode(node); // mới tạo set node pre = start node
        p.setFrUser(frParam == null ? frUser.getId() : frParam);
        p.setOrgName(org.getName());
        p.setDocId(docId);
        p.setHandleStatus(handleStatus);
        p.setHandleType(handleType);
        p.setStep(step);
        p.setTransferStep(transferStep);
        p.setToUser(toUsers.getId());
        if (deadline != null) {
            p.setDeadline(deadline);
        }
        p.setDelegaterId(node == Constant.START_NODE ? null : delegateId);
        return getRepository().save(p);
    }

    public DocumentInProcess save(Long node, Long docId, DocumentInHandleStatusEnum handleStatus,
                                  HandleTypeEnum handleType, Integer step, Integer transferStep, User toUsers, Long frParam,
                                  Long assignHandle, Date deadline, Long delegateId, User frUser, Long clientId) {
        Organization org = orgService.validOrgId(clientId, toUsers.getOrg());
        DocumentInProcess o = findByDocIdAndStepAndHandleTypeAndToUser(docId, step, toUsers.getId(), handleType, clientId);
        DocumentInProcess p = o == null ? new DocumentInProcess() : o;
        p.setNode(node);
        p.setPreNode(node); // mới tạo set node pre = start node
        p.setFrUser(frParam == null ? frUser.getId() : frParam);
        p.setOrgName(org.getName());
        p.setDocId(docId);
        p.setHandleStatus(handleStatus);
        p.setHandleType(handleType);
        p.setStep(step);
        p.setTransferStep(transferStep);
        p.setToUser(toUsers.getId());
        if (deadline != null) {
            p.setDeadline(deadline);
        }
        p.setDelegaterId(node == Constant.START_NODE ? null : delegateId);
        return getRepository().save(p);
    }

    public DocumentInProcess findByToUserAndDocId(Long userId, Long docId) {
        List<DocumentInProcess> rs = processRepository.findByToUserAndDocId(docId, userId,
                BussinessCommon.getClientId());
        return rs.isEmpty() ? null : rs.get(0);
    }

    public Boolean checkDocumentIsStatus(int step, Long docId) {
        List<DocumentInProcess> rs = processRepository.getDocumentInProcessByDocIdAndStatusAndStep(docId, step,
                BussinessCommon.getClientId());
        if (rs.size() > 0) {
            if (rs.get(0).getHandleStatus() == DocumentInHandleStatusEnum.CHO_XU_LY) {
                return false;
            } else {
                return true;
            }
        } else {
            return false;
        }
    }

    public DocumentInProcess findByToUserAndDocId2(Long userId, Long docId) {
        List<DocumentInProcess> rs = processRepository.findByToUserAndDocId2(docId, userId,
                BussinessCommon.getClientId());
        return rs.isEmpty() ? null : rs.get(0);
    }

    public List<DocumentInProcess> findHandlesInSameStepExclusive(Long frUser, Long userId, Long docId, int step, List<DocumentInHandleStatusEnum> handleStatus) {
        return processRepository.findHandlesInSameStepExclusive(frUser ,userId, docId, step, handleStatus, BussinessCommon.getClientId());
    }

    public DocumentInProcess findByToUsersAndDocId2(List<Long> userId, Long docId) {
        List<DocumentInProcess> rs = processRepository.findByToUsersAndDocId2(docId, userId,
                BussinessCommon.getClientId());
        return rs.isEmpty() ? null : rs.get(0);
    }

    public DocumentInProcess findByRelatedAndDocId(Long userId, Long docId, HandleTypeEnum type) {
        List<DocumentInProcess> rs = processRepository.findByRelatedAndDocId(docId, userId,
                BussinessCommon.getClientId());
        if (BussinessCommon.isEmptyList(rs)) {
            return null;
        }
        if (type == null) {
            return rs.get(0);
        }
        for (DocumentInProcess p : rs) {
            if (p.getHandleType().equals(type)) {
                return p;
            }
        }
        return null;
    }

    public DocumentInProcess findByDelegateAndDocId(Long userId, Long docId, HandleTypeEnum type) {
        List<DocumentInProcess> rs = processRepository.findByDelegateAndDocId(docId, userId,
                BussinessCommon.getClientId());
        if (BussinessCommon.isEmptyList(rs)) {
            return null;
        }
        if (type == null) {
            return rs.get(0);
        }
        for (DocumentInProcess p : rs) {
            if (p.getHandleType().equals(type)) {
                return p;
            }
        }
        return null;
    }

    public List<DocumentInProcess> getListProcessByDocId(Long docId) {
        return processRepository.getListProcessByDocId(docId, BussinessCommon.getClientId());
    }

    public List<DocumentInProcess> updateByActive(Long docId, boolean active) {
        List<DocumentInProcess> pList = getListProcessByDocId(docId);
        pList.forEach(i -> {
            i.setActive(active);
        });
        return getRepository().saveAll(pList);
    }

    public DocumentInProcess updateForDelegate(Long docId, Long toUser, String tab) {
        HandleTypeEnum type = null;
        User user = BussinessCommon.getUser();
        if (tab != null) {
            if ("xu_ly_chinh".equals(tab)) {
                type = HandleTypeEnum.MAIN;
            }
            if ("xu_ly_phoi_hop".equals(tab)) {
                type = HandleTypeEnum.SUPPORT;
            }
        }
        DocumentInProcess oProcess = findByDelegateAndDocId(user.getId(), docId, type);
        if (oProcess == null || !isStatus(oProcess.getHandleStatus(), HANDLE_STATUS_NOT_YET)
                || isType(HandleTypeEnum.SHOW, oProcess.getHandleType())) {
            throw new RestExceptionHandler(Message.NO_DONE_PROCESS);
        }
        // Check review required
        validateReviewRequired(oProcess);

        updateProcess(docId, oProcess);
        if (type != null) {
            saveFrDelegate2(oProcess, type, DocumentInHandleStatusEnum.DA_XU_LY_UQ);
            trackingService.save(docId, DocumentInTrackingEnum.FINISH_UQ, user.getId());
        }
        return oProcess;
    }

    private DocumentInProcess validOldProcess(Long docId, Long toUser) {
        DocumentInProcess oProcess = findByToUserAndDocId2(toUser, docId);

        if (oProcess == null) {
            throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);
        }

        // #Ticket 2165
        // Add done status can done document
        if (!HANDLE_STATUS_DONE.contains(oProcess.getHandleStatus())) {
            throw new RestExceptionHandler(Message.NO_DONE_PROCESS);
        }

        return oProcess;
    }

    public DocumentInProcess update(Long docId, DocumentInProcess oProcess) {

        // Check review required
        validateReviewRequired(oProcess);

        updateProcess(docId, oProcess);
        return oProcess;
    }

    public List<DocumentInProcess> updateDone(Long docId) {
        List<DocumentInProcess> listProcess = getListProcessByDocId(docId);
        listProcess.forEach(i -> {
            if (!DocumentInHandleStatusEnum.DA_XU_LY_SWITCH.equals(i.getHandleStatus())) {
                i.setProgress(100);
                i.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);
            }
        });

        return getRepository().saveAll(listProcess);
    }

    public void validateReviewRequired(DocumentInProcess oProcess) {
        if (oProcess == null)
            return;
        Boolean review = bpmnService.getReviewRequiredByNodeId(oProcess.getNode());
        if (HandleTypeEnum.MAIN.equals(oProcess.getHandleType()) && checkReviewRequired(oProcess, review))
            throw new RestExceptionHandler(Message.REVIEW_REQUIRED);
    }

    private boolean checkReviewRequired(DocumentInProcess oProcess, Boolean review) {
//		NodeModel2 node = p.getNodeObj();
//		return reviewRequired && node != null && p.getNode() != null
//				&& Boolean.TRUE.equals(node.getReviewRequired()) && !Boolean.TRUE.equals(p.getReview());

        Long nodeId = oProcess.getNode();
        if (reviewRequired && nodeId != null) {
            if ((Boolean.TRUE.equals(review) || Boolean.TRUE.equals(oProcess.getRequestReview()))
                    && !Boolean.TRUE.equals(oProcess.getReview())) {
                return true;
            }
        }
        return false;
    }

    @Transactional
    public void updateAuto(List<Long> docIdList) {
        List<DocumentInProcess> pfirstList = findByDocIdAndLastedStep(docIdList);
        if (!BussinessCommon.isEmptyList(pfirstList)) {
            for (DocumentInProcess p : pfirstList) {
                updateProcess(p.getDocId(), p, isType(HandleTypeEnum.MAIN, p.getHandleType()) ? p.getToUser() : null);
            }
        }
    }

    private void updateProcess(Long docId, DocumentInProcess oProcess, Long toUser) {
        Documents doc = updateDocByStatus(docId, DocumentStatusEnum.DONE);
        List<DocumentInProcess> prList = update(docId, oProcess.getStep(), DocumentInHandleStatusEnum.DA_XU_LY, toUser,
                Constant.OTHER_ACTION_TYPE);
        List<User> userIds = prList.stream().map(DocumentInProcess::getToUsers).collect(Collectors.toList());
        trackingService.save(doc, DocumentInTrackingEnum.FINISH, userIds, null, Constant.OTHER_ACTION_TYPE);
        noticeService.setActiveByDocIdAndDocType(docId, DocumentTypeEnum.VAN_BAN_DEN, false);
    }

    private void updateProcess(Long docId, DocumentInProcess old) {
        if (isType(HandleTypeEnum.MAIN, old.getHandleType())) {

            // #Ticket 2165
            // 1. Remove check last node
            // 2. Add done status can done document

            // update old process, tracking
            old.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);

            // close record remain
            closeRemainProcessByUserId(docId, old.getToUser());

            saveTrackingByType(old);

        } else if (isSupportRoleDone(old)) { // #2750 Vai trò phối hợp đã chuyển xử lý rồi mong muốn đóng record xuất phát từ record này
            // Đóng record của người hiện tại
            old.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);
            saveTrackingByType(old);
            informByType(old);
        } else {
            old.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);
            saveTrackingByType(old);
            informByType(old);
        }
        closeDoc(docId, old);
        closeBranch(docId, old);

        // Set end task
        old.setEndTask(true);
        processRepository.save(old);
    }

    /**
     * Close branch by all role
     *
     * @param docId
     * @param old
     */
    private void closeBranch(Long docId, DocumentInProcess old) {
        // #2769 Thêm cấu hình ở luồng cho phép Văn bản mà lãnh đạo Cục đã hoàn thành
        // trưởng phòng có thể chuyển xử lý tiếp cho cán bộ
        if (Boolean.TRUE.equals(bpmnService.isForceCloseBranchByNodeId(old.getNode()))) {
            // Đóng record mà người hiện tại đã chuyển xử lý tới
            List<DocumentInProcess> list = userTransferTo(old, docId);
            list.forEach(i -> {
                i.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);
                i.setCloseBranch(true);
                i.setEndTask(true);
                processRepository.save(i);
            });
            old.setCloseBranch(true);
            processRepository.save(old);
        }
    }

    /**
     * #2750
     * Kiểm tra vai trò xử lý phối hợp đã xử lý hay chưa ?
     *
     * @param old
     * @return
     */
    private boolean isSupportRoleDone(DocumentInProcess old) {
        return HandleTypeEnum.SUPPORT.equals(old.getHandleType())
                && DocumentInHandleStatusEnum.DA_XU_LY.equals(old.getHandleStatus());
    }

    /**
     * Target : Do close document
     * Condition 1 AND 2
     * 1. Flag : mainRequire = false
     * 2. Process of last step has not main
     * 3. Record of cur user is last record that not done
     *
     * @param docId
     */
    public void closeDoc(Long docId, DocumentInProcess old) {

        // Verify condition 1
        if (this.mainRequire || old == null)
            return;

        Documents doc = docRepository.findByClientIdAndId(BussinessCommon.getClientId(), docId);
        if (DocumentStatusEnum.DONE.equals(doc.getStatus())) {
            return;
        }

        if (old.getHandleType().equals(HandleTypeEnum.INTERNAL_INCOMING)) {
            List<DocumentInHandleStatusEnum> notYetHandledStatus = Arrays.asList(DocumentInHandleStatusEnum.CHO_XU_LY, DocumentInHandleStatusEnum.DANG_XU_LY);
            List<DocumentInProcess> processesInSameStepExclusive = findHandlesInSameStepExclusive(old.getFrUser(), old.getToUser(), old.getDocId(), old.getStep(), notYetHandledStatus);
            if (!processesInSameStepExclusive.isEmpty()) {
                for (DocumentInProcess dip : processesInSameStepExclusive) {
                    dip.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);
                    trackingService.save(doc.getId(), DocumentInTrackingEnum.FINISH, dip.getToUser());
                }
                getRepository().saveAll(processesInSameStepExclusive);
            }
        }

        // Verify condition 2, 3
        long count = processRepository.countNotYetUser(docId, BussinessCommon.getClientId());

        // Update doc status
        if (count == 0 || old.getHandleType().equals(HandleTypeEnum.INTERNAL_INCOMING)) {
            doc.setStatus(DocumentStatusEnum.DONE);
            docRepository.save(doc);

            if (doc.getParentId() != null) {
                // Add thông báo
                DocumentInProcess parent = processRepository.findFirstByDocIdAndHandleStatusAndActive(doc.getParentId(),
                        DocumentInHandleStatusEnum.CHUYEN_DON_VI, true);
                if (parent != null) {
                    if (parent.getToUser() != null) {
                        noticeService.add(parent.getToUser(), doc.getParentId(), doc.getPreview(),
                                DocumentTypeEnum.VAN_BAN_DEN, NotificationHandleStatusEnum.DV_HOAN_THANH,
                                ModuleCodeEnum.DOC_OUT_MAIN);
                    }
                    if (parent.getDelegaterId() != null) {
                        noticeService.add(parent.getDelegaterId(), doc.getParentId(), doc.getPreview(),
                                DocumentTypeEnum.VAN_BAN_DEN, NotificationHandleStatusEnum.DV_HOAN_THANH,
                                ModuleCodeEnum.DOC_IN_DELEGATE);
                    }
                }
            }

            //Mark person who closed doc
            old.setClose(true);
            processRepository.save(old);
        }
    }

    private void saveTrackingByType(DocumentInProcess p) {
        Long userId = BussinessCommon.getUserId();
        DocumentInTrackingEnum trackingSts = DocumentInTrackingEnum.FINISH;
        if (userId.equals(p.getDelegaterId())) {
            trackingSts = DocumentInTrackingEnum.FINISH_UQ;
        }
        trackingService.add(new DocumentInTracking(p.getDocId(), userId, trackingSts, p.getOrgName(),
                p.getDocument().getDocType().getName()));
    }

    private void informByType(DocumentInProcess p) {
        Long userId = BussinessCommon.getUserId();
        NotificationHandleStatusEnum noti;
        switch (p.getHandleType()) {
            case SUPPORT:
                noti = NotificationHandleStatusEnum.PHOI_HOP;
                if (userId.equals(p.getToUser())) {
                    noti = NotificationHandleStatusEnum.PHOI_HOP_UQ;
                }
                noticeService.setActiveForUserHandle(userId, p.getDocId(), DocumentTypeEnum.VAN_BAN_DEN, false, noti,
                        ModuleCodeEnum.DOC_OUT_COMBINE);
                break;
            case DIRECTION:
                noti = NotificationHandleStatusEnum.CHI_DAO;
                noticeService.setActiveForUserHandle(userId, p.getDocId(), DocumentTypeEnum.VAN_BAN_DEN, false, noti,
                        ModuleCodeEnum.DOC_OUT_MAIN);
                break;
            case SHOW:
                noti = NotificationHandleStatusEnum.NHAN_DE_BIET;
                noticeService.setActiveForUserHandle(userId, p.getDocId(), DocumentTypeEnum.VAN_BAN_DEN, false, noti,
                        ModuleCodeEnum.DOC_OUT_KNOW);
                break;
            default:
                break;
        }
    }

    public Documents updateDocByStatus(Long docId, DocumentStatusEnum status) {
        Documents doc = docRepository.findByClientIdAndId(BussinessCommon.getClientId(), docId);
        if (doc == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_DOC);
        }
        doc.setStatus(status);
        return docRepository.save(doc);
    }


    public Documents updateDocByStatus(Long docId, DocumentStatusEnum status, Long node, Long preNode) {
        Documents doc = docRepository.findByClientIdAndId(BussinessCommon.getClientId(), docId);
        if (doc == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_DOC);
        }
        doc.setStatus(status);
        if (DocumentStatusEnum.RETURN_DOC.equals(status)) {
            doc.setNode(Constant.START_NODE.equals(node) ? null : node);
            doc.setPreNode(Constant.START_NODE.equals(preNode) ? null : preNode);
        }

        return docRepository.save(doc);
    }

    /***
     * Đóng những record tồn đọng đối với user id được truyền vào
     *
     * @param docId
     * @param toUserId
     */
    private void closeRemainProcessByUserId(Long docId, Long toUserId) {
        processRepository.closeRemainProcessByUserId(docId, toUserId, BussinessCommon.getClientId());
    }

    public static <T> boolean isStatus(T status, T[] arr) {
        if (arr == null || arr.length == 0 || status == null) {
            return false;
        }
        for (T st : arr) {
            if (status.equals(st)) {
                return true;
            }
        }
        return false;
    }

    public boolean isStatus(DocumentInHandleStatusEnum status, DocumentInHandleStatusEnum statusEnum) {
        if (status == null || statusEnum == null) {
            return false;
        }
        return statusEnum.equals(status);
    }

    public boolean isType(HandleTypeEnum type, HandleTypeEnum typeEnum) {
        if (typeEnum == null || type == null) {
            return false;
        }
        return typeEnum.equals(type);
    }

    public DocumentInProcess findByDocIdAndFirstStep(Long docId) {
        List<DocumentInProcess> rsList = processRepository.findByDocIdAndFirstStep(docId,
                BussinessCommon.getClientId());
        return rsList.isEmpty() ? null : rsList.get(0);
    }

    public List<DocumentInProcess> findByDocIdAndStep(Long docId, Integer step) {
        return processRepository.findByDocIdAndStepAndClientIdAndActive(docId, step, BussinessCommon.getClientId(),
                true);
    }

    public List<DocumentInProcess> findByDocIdAndStep(Long docId, Integer step, HandleTypeEnum type) {
        return processRepository.findByDocIdAndStepAndClientIdAndActiveAndHandleType(docId, step,
                BussinessCommon.getClientId(), true, type);
    }

    /**
     * for done with all type handle
     *
     * @param prList
     * @param status
     */
    public List<DocumentInProcess> update(Long docId, Integer step, DocumentInHandleStatusEnum status, Long main,
                                          int type) {
        List<DocumentInProcess> prList = findByDocIdAndStep(docId, step);
        for (DocumentInProcess i : prList) {
            if (DocumentInHandleStatusEnum.DA_XU_LY_SWITCH.equals(i.getHandleStatus()))
                continue;
            i.setHandleStatus(getActionStatus(i.getToUser(), main, status, type));
            if (DocumentInHandleStatusEnum.DA_XU_LY.equals(status)
                    || DocumentInHandleStatusEnum.DA_XU_LY_UQ.equals(status)) {
                i.setProgress(100);
            }
        }

        return getRepository().saveAll(prList);
    }

    /**
     * fb 10/12/2020 xử lý chính chuyển xử lý -> chỉ main done , phối hợp + nhận để
     * biết không liên quan
     *
     * @param docId
     * @param step
     * @param status
     * @param main
     * @param type
     * @return
     */
    public List<DocumentInProcess> update(Long docId, Integer step, DocumentInHandleStatusEnum status,
                                          HandleTypeEnum type) {
        List<DocumentInProcess> prList = findByDocIdAndStep(docId, step, type);
        for (DocumentInProcess i : prList) {
            if (DocumentInHandleStatusEnum.DA_XU_LY_SWITCH.equals(i.getHandleStatus()))
                continue;
            i.setHandleStatus(status);
            if (DocumentInHandleStatusEnum.DA_XU_LY.equals(status)
                    || DocumentInHandleStatusEnum.DA_XU_LY_UQ.equals(status)) {
                i.setProgress(100);
            }
        }

        return getRepository().saveAll(prList);
    }

    public DocumentInHandleStatusEnum getActionStatus(Long user, Long main, DocumentInHandleStatusEnum status,
                                                      int type) {
        if (!user.equals(main)) {
            return status;
        }
        if (type == Constant.TRANSFER_HANDLE_TYPE) {
            return DocumentInHandleStatusEnum.DA_XU_LY;
        }
        if (type == Constant.RETURN_DOC_TYPE) {
            return DocumentInHandleStatusEnum.DA_TRA_LAI;
        }
        if (type == Constant.ORG_TRANSFER_TYPE) {
            return DocumentInHandleStatusEnum.CHUYEN_DON_VI;
        }
        return status;
    }

    public DocumentInProcess update(DocumentInProcess oProcess, DocumentInHandleStatusEnum status) {
        oProcess.setHandleStatus(status);
        if (DocumentInHandleStatusEnum.DA_XU_LY.equals(status)
                || DocumentInHandleStatusEnum.DA_XU_LY_UQ.equals(status)) {
            oProcess.setProgress(100);
        }
        return getRepository().save(oProcess);
    }

    public List<DocumentInProcess> update(Long docId, Integer step, DocumentInHandleStatusEnum statusMain,
                                          DocumentInHandleStatusEnum status) {
        List<DocumentInProcess> oProcessList = findByDocIdAndStep(docId, step);
        for (DocumentInProcess i : oProcessList) {
            if (DocumentInHandleStatusEnum.DA_XU_LY_SWITCH.equals(i.getHandleStatus()))
                continue;
            i.setHandleStatus(HandleTypeEnum.MAIN.equals(i.getHandleType()) ? statusMain : status);
            if (DocumentInHandleStatusEnum.DA_XU_LY.equals(i.getHandleStatus())
                    || DocumentInHandleStatusEnum.DA_XU_LY_UQ.equals(i.getHandleStatus())) {
                i.setProgress(100);
            }
        }

        return getRepository().saveAll(oProcessList);
    }

    public List<DocumentInProcess> update(List<DocumentInProcess> oProcessList, DocumentInHandleStatusEnum status, Long nextNode) {
        User u = BussinessCommon.getUser();
        Integer progress = 0;

        for (DocumentInProcess i : oProcessList) {
            if (DocumentInHandleStatusEnum.DA_XU_LY_SWITCH.equals(i.getHandleStatus()))
                continue;
            i.setToUser(u.getId());
            i.setFrUser(u.getId());
            i.setHandleStatus(status);
            i.setTransfer(true);
            i.setNextNode(nextNode);
            i.setEndTask(null);
            i.setCloseBranch(null);
            progress = setProgress(status);
            if (progress != null) {
                i.setProgress(progress);
            }
        }
        return getRepository().saveAll(oProcessList);
    }

    private Integer setProgress(DocumentInHandleStatusEnum status) {
        if (DocumentInHandleStatusEnum.CHO_XU_LY.equals(status)) {
            return 0;
        }
        if (DocumentInHandleStatusEnum.DA_XU_LY.equals(status)
                || DocumentInHandleStatusEnum.DA_XU_LY_UQ.equals(status)) {
            return 100;
        }
        return null;
    }

    public List<DocumentInProcess> findByDocIdAndStepAndToUsers(Long docId, Integer step, List<Long> idUs) {
        return processRepository.findByDocIdAndStepAndToUsers(docId, step, idUs, BussinessCommon.getClientId());
    }

    public DocumentInProcess findByDocIdAndStepAndHandleTypeAndToUser(Long docId, Integer step, Long toUser,
                                                                      HandleTypeEnum handleType) {
        return processRepository.findByDocIdAndStepAndHandleTypeAndToUser(docId, step, toUser, handleType,
                BussinessCommon.getClientId());
    }

    public DocumentInProcess findByDocIdAndStepAndHandleTypeAndToUser(Long docId, Integer step, Long toUser,
                                                                      HandleTypeEnum handleType, Long clientId) {
        return processRepository.findByDocIdAndStepAndHandleTypeAndToUser(docId, step, toUser, handleType,
                clientId);
    }

    public DocumentInProcess findLastProcessByDocIdAndHandleTypeAndToUserOrDelegaterAndHandleStatus(Long docId,
                                                                                                    Long toUser, HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus) {
        List<DocumentInProcess> result = processRepository.findByDocIdAndHandleTypeAndToUserOrDelegaterAndHandleStatus(
                docId, toUser, handleType, handleStatus, BussinessCommon.getClientId());
        if (result.isEmpty())
            return null;
        return result.get(0);
    }

    public DocumentInProcess findByDocIdAndStepAndHandleTypeAndDelegaterId(Long docId, Integer step, Long delegaterId,
                                                                           HandleTypeEnum handleType) {
        return processRepository.findByDocIdAndStepAndHandleTypeAndDelegaterId(docId, step, delegaterId, handleType,
                BussinessCommon.getClientId());
    }

    public List<DocumentInProcess> saveList(User u, Long preNode, Long node, List<HandlerDto> handlers,
                                            HandleTypeEnum handleType, Long docId, Integer step, Date deadline, Integer transferStep, boolean isMain, Boolean requestReview) {
        if (BussinessCommon.isEmptyList(handlers)) {
            return Collections.emptyList();
        }
        List<Long> assignList = handlers.stream().map(HandlerDto::getAssignHandler).map(User::getId)
                .collect(Collectors.toList());
        List<DocumentInProcess> oList = findByDocIdAndStepAndToUsers(docId, step, assignList);
        List<DocumentInProcess> pList = new ArrayList<>();
        handlers.forEach(i -> {
            if (i.getAssignHandler() != null) {
                Optional<DocumentInProcess> op = oList.stream()
                        .filter(o -> o.getToUser().equals(i.getAssignHandler().getId())).findFirst();
                DocumentInProcess p = op.isPresent() ? op.get() : new DocumentInProcess();
                p.setNode(node);
                p.setPreNode(preNode);
                p.setFrUser(u.getId());
                p.setOrgName(u.getOrgModel().getName());
                p.setDocId(docId);
                p.setHandleType(handleType);
                p.setHandleStatus(DocumentInHandleStatusEnum.CHO_XU_LY);
                p.setStep(step);
                p.setToUser(i.getAssignHandler().getId());
                p.setDelegaterId(i.getDelegateHandler() != null ? i.getDelegateHandler().getId() : null);
                p.setDeadline(deadline);
                p.setTransferStep(transferStep);
                p.setDirection(isMain);
                p.setRequestReview(requestReview);
                pList.add(p);
            }
        });

        return getRepository().saveAll(pList);
    }

    public List<DocumentInProcess> saveList1(Long node, List<Long> users, HandleTypeEnum handleType, Long docId,
                                             Integer step) {
        List<DocumentInProcess> oList = findByDocIdAndStepAndToUsers(docId, step, users);
        if (BussinessCommon.isEmptyList(users)) {
            return new ArrayList<>();
        }
        User u = BussinessCommon.getUser();
        List<DocumentInProcess> pList = new ArrayList<>();
        users.forEach(i -> {
            Optional<DocumentInProcess> op = oList.stream().filter(o -> o.getToUser().equals(i)).findFirst();
            DocumentInProcess p = op.isPresent() ? op.get() : new DocumentInProcess();
            p.setNode(node);
            p.setFrUser(u.getId());
            p.setOrgName(u.getOrgModel().getName());
            p.setDocId(docId);
            p.setHandleType(handleType);
            p.setHandleStatus(DocumentInHandleStatusEnum.CHO_XU_LY);
            p.setStep(step);
            p.setToUser(i);
            p.setDelegaterId(null);
            pList.add(p);
        });

        return getRepository().saveAll(pList);
    }

    /**
     * default is handle status is done
     *
     * @param news
     * @return
     */
    @Transactional
    public DocumentInProcess done(Long docId, String comment, MultipartFile[] files, Date signDate) {
        User u = BussinessCommon.getUser();

        DocumentInProcess oProcess = validOldProcess(docId, u.getId());
        // #2166 && encryption file to uncheck for case
//		if (HandleTypeEnum.MAIN.equals(oProcess.getHandleType())) {
//			BussinessCommon.require("Ý kiến xử lý", comment);
//		}
        if (signDate != null) {
            oProcess.setSignDate(signDate);
            comment += " - Ngày ký: " + DateTimeUtils.getDateNotTime(signDate);
        }

        return handleDoneDocument(docId, oProcess, comment, files);
    }

    private DocumentInProcess handleDoneDocument(Long docId, DocumentInProcess old, String comment, MultipartFile[] files) {
        BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);

        // update status
        DocumentInProcess pdoc = update(docId, old);

        // add comment & attachment
        if (StringUtils.isNullOrEmpty(comment) && BussinessCommon.isEmptyArr(files)) {
            return pdoc;
        }
        DocumentComment cmt = new DocumentComment();
        cmt.setDocId(docId);
        cmt.setComment(comment);
        cmt.setType(DocumentCommentTypeEnum.HOAN_THANH_XU_LY);
        cmt = cmtService.saveCmt(cmt);
        attachCmtService.addListAttachmentComment(files, cmt.getId());
        return pdoc;
    }

    public List<TrackingSendReceiveDto> listTracking(Long docId) {
        User u = BussinessCommon.getUser();

        List<DocumentInProcess> allList = getListProcessByDocId(docId);
        List<DocumentInProcess> transferList = getListByCondition(allList, true);
        List<TrackingSendReceiveDto> rslist = new ArrayList<>();

        transferList.forEach(t -> {
            TrackingSendReceiveDto ts = new TrackingSendReceiveDto();
            ts.setDate(DateTimeUtils.getLastDate(t.getCreateDate(), t.getUpdateDate()));
            ts.setFullName(t.getFrUsers().getFullName());
            ts.setPosition(t.getFrUsers().getPositionModel().getName());

            List<TrackingReceiveDocDto> trList = new ArrayList<>();
            int no = 0;
            for (DocumentInProcess r : allList) {
                if (r.getFrUser().equals(t.getFrUser()) && r.getStep().equals(t.getStep())) {
                    TrackingReceiveDocDto tr = new TrackingReceiveDocDto();
                    tr.setNo(++no);
                    tr.setAction(BussinessCommon.getAction(r.getHandleStatus(), readToDoing));
                    tr.setOrg(r.getOrgName());
                    tr.setFullNameRe(r.getToUsers() != null ? r.getToUsers().getFullName() : null);
                    tr.setStatusHandle(BussinessCommon.getTypeEnum(r.getHandleType(), r.getDirection()));
                    tr.setPositionRe(r.getToUsers() != null && r.getToUsers().getPositionModel() != null
                            ? r.getToUsers().getPositionModel().getName()
                            : null);
                    tr.setProgress(r.getProgress());
                    tr.setComment(r.getComment());
                    tr.setDeadline(setDeadline(r.getDeadline(), u.getId(), r));
                    trList.add(tr);
                }
            }
            ts.setReceiveList(trList);
            rslist.add(ts);
        });
        return rslist;
    }

    // only view deadline when current_user is assigned or current_user is assigner
    private Date setDeadline(Date deadline, Long currentUser, DocumentInProcess p) {
        if (currentUser.equals(p.getFrUser()) || currentUser.equals(p.getToUser())) {
            return deadline;
        }
        return null;
    }

    public List<DocumentInProcess> getListByCondition(List<DocumentInProcess> allList, boolean isTranfer) {
        if (allList == null || allList.isEmpty()) {
            return new ArrayList<>();
        }
        List<DocumentInProcess> tranferList = new ArrayList<>();
        Map<PDocDto, DocumentInProcess> map = new HashMap<>();
        allList.forEach(i -> {
            PDocDto key = new PDocDto(i.getFrUser(), i.getHandleType(), i.getStep());
            if (isType(i.getHandleType(), HandleTypeEnum.MAIN) && !i.getToUser().equals(i.getFrUser())) {
                map.put(key, i);
            }
        });
        map.forEach((k, v) -> tranferList.add(v));
        tranferList.sort(Comparator.comparing(DocumentInProcess::getStep));
        if (isTranfer) {
            return tranferList;
        }
        return allList;
    }

    public boolean isStatusInArray(DocumentInHandleStatusEnum[] values, DocumentInHandleStatusEnum status) {
        return Arrays.stream(values).anyMatch(status::equals);
    }

    public ProcessDocumentDto getActionByDoc(Documents doc) {
        Long docId = doc.getId();
        ProcessDocumentDto dto = new ProcessDocumentDto();
        Long userId = BussinessCommon.getUserId();
        User u = BussinessCommon.getUser();
        Category positionVanThuBan = categoryService.findByName(u.getClientId(), Constant.VAN_THU_MAIN);
        List<Long> userIds = new ArrayList<>();
        if (positionVanThuBan != null && u.getPositionModel() != null && (u.getPositionModel().getId().longValue() == positionVanThuBan.getId().longValue())) {
            userIds = userRepository.getUsersByNameCategory(Constant.VAN_THU_MAIN, u.getOrg());
            if (userIds.isEmpty()) {
                throw new RestExceptionHandler(Message.EMPTY_CLERICAL_HANDLE);
            }
        } else {
            userIds.add(userId);
        }
        DocumentInProcess old = findByToUsersAndDocId2(userIds, docId);

        // cho ý kiến không cần phải có trong luồng
        dto.setCanReply(manipulationService.hasAskToUser(docId, userId));
        dto.setDocId(docId);
        dto.setMergedLines(doc.getMergedLines());
        if (old == null) {
            return dto;
        }

        boolean isDoneDoc = DocumentStatusEnum.DONE.equals(doc.getStatus());
        dto.setCanAsk(canAsk(old, isDoneDoc));
        dto.setNode(old.getNode());
        if (old.getNextNode() != null) {
            dto.setNextNode(old.getNextNode());
        }
        // check can retake
        dto.setCanRetake(canRetake(old));

        Boolean canRqReview = false;
        Boolean canReview = false;
        if (DocumentInHandleStatusEnum.CHO_DANH_GIA.equals(old.getHandleStatus())) {
            canReview = true;
            dto.setCanReview(canReview);
        } else {
            canRqReview = canRequestReview(old, bpmnService.getReviewRequiredByNodeId(old.getNode()));
            dto.setCanRequestReview(canRqReview);
        }

        List<NodeDto> nodes = bpmnService.nodeList(old.getNode(), TYPE_DOCUMENT.INCOMING); // list next node
        boolean lastNode = lastNode(nodes);
        Boolean closeBranchByNodeId = bpmnService.getCloseBranchByNodeId(old.getNode());
        dto.setCanTransfer(canTransfer(old, nodes, canReview || canRqReview));
        dto.setCanReturn(canReturn(old, isDoneDoc));
        dto.setCanDone(canDone(old, canReview || canRqReview, lastNode, isDoneDoc, doc.getMergedLines(), closeBranchByNodeId));
        dto.setStep(old.getStep());
        dto.setCanSwitchOrAdd(canSwitchOrAdd(old));
        dto.setCanRetakeDone(!dto.isCanDone() && canRetakeDone(old, doc));
        dto.setCanFinish(DocumentService.checkCanFinish(doc, old.getHandleStatus()));
        dto.setCanOrgTransfer(canOrgTransfer(old, nodes, canReview || canRqReview, isDoneDoc));
        dto.setAllowConfig(canUpdateDeadline(old));
        dto.setCanRead(canRead(old, doc));
        dto.setType(old.getHandleType());
        return dto;
    }

//	/**
//	 * check node is last node
//	 * @param nodes
//	 * @param node
//	 * @return
//	 */
//	public boolean isLastNodeReal(List<NodeDto> nodes, Long node) {
//		for (NodeDto i : nodes) {
//			if (node != null && node.equals(i.getId()) && Boolean.TRUE.equals(i.isLastNode())) {
//				return true;
//			}
//		}
//		return false;
//	}

    public boolean review(List<NodeDto> reviewNodes) {
        return reviewNodes.stream().filter(i -> Boolean.TRUE.equals(i.isReviewRequired())).count() > 0;
    }

    public boolean bpmnError(List<NodeDto> nodes) {
        return nodes.stream().filter(i -> Boolean.FALSE.equals(i.isBpmnActive())).count() > 0;
    }

    public boolean lastNode(List<NodeDto> nodes) {
        return nodes.stream().filter(i -> i.isLastNode()).count() > 0;
    }

    public boolean canReview(DocumentInProcess oProcess) {
        return oProcess != null && DocumentInHandleStatusEnum.CHO_DANH_GIA.equals(oProcess.getHandleStatus());
    }

    public boolean canRead(DocumentInProcess p, Documents doc) {
        if (p == null || doc == null)
            return false;
        return !DocumentStatusEnum.RETAKE_DOC.equals(doc.getStatus()) && isType(p.getHandleType(), HandleTypeEnum.SHOW)
                && Arrays.asList(HANDLE_STATUS_NOT_YET).contains(p.getHandleStatus());
    }

    public boolean canRetakeDone(DocumentInProcess p, Documents doc) {
        if (p == null)
            return false;

//		boolean isDoneDoc = DocumentStatusEnum.DONE.equals(doc.getStatus());
        boolean isDoneHandle = isStatusInArray(DONE, p.getHandleStatus());

        if (!isDoneHandle) {
            return false;
        }

        // #Ticket 2165
        // Is not last node can done document
        // 14/4/2022 : End task can retake done
        if (Boolean.TRUE.equals(p.getClose()) || Boolean.TRUE.equals(p.getEndTask())) {
            return true;
        }

        return isDoneHandle && !Boolean.TRUE.equals(p.getTransfer());
    }

    public boolean canAsk(DocumentInProcess p, boolean isDoneDoc) {
        if (p == null || isDoneDoc)
            return false;
        return !isStatusInArray(NOT_IN_DONE, p.getHandleStatus());
    }

    public boolean canRetake(DocumentInProcess p) {
        if (p == null || p.getDocument() == null)
            return false;
        List<DocumentInHandleStatusEnum> exeSts = Arrays.asList(DocumentInHandleStatusEnum.DA_XU_LY,
                DocumentInHandleStatusEnum.DA_XU_LY_ADD_USER);
        return DocumentStatusEnum.DOING.equals(p.getDocument().getStatus()) && exeSts.contains(p.getHandleStatus())
                && p.getTransferStep() > 0;
    }

    public boolean canReview2(DocumentInProcess old) {
        return DocumentInHandleStatusEnum.CHO_DANH_GIA.equals(old.getHandleStatus());
    }

    public boolean canRequestReview(DocumentInProcess old, Boolean review) {
        if (old == null)
            return false;

        List<DocumentInHandleStatusEnum> exeSts = Arrays.asList(DocumentInHandleStatusEnum.DANG_XU_LY,
                DocumentInHandleStatusEnum.CHO_XU_LY, DocumentInHandleStatusEnum.DA_XU_LY_ADD_USER);
        return (((exeSts.contains(old.getHandleStatus())) && old.getReview() == null
                && checkReviewRequired(old, review))
                || DocumentInHandleStatusEnum.DG_TU_CHOI.equals(old.getHandleStatus()));
    }

    /**
     * If old record has request review, and result is ok => pass
     *
     * @param old
     * @return
     */
    private boolean passReview(DocumentInProcess old) {
        if (old == null)
            return true;

        if (Boolean.TRUE.equals(old.getRequestReview()) && !Boolean.TRUE.equals(old.getReview())) {
            return false;
        }

        if (DocumentInHandleStatusEnum.XIN_DANH_GIA.equals(old.getHandleStatus()))
            return false;

        return true;
    }

    public boolean canUpdateDeadline(DocumentInProcess p) {
        User u = BussinessCommon.getUser();
        Long org = u.getOrg();
        List<Long> setOrg = orgService.listParentOrg(org);
        if (p == null)
            return false;
        Set<Long> setNodeValid = nodeRepo.validNode(p.getNode(), org, u.getPosition(), u.getId(), setOrg);
        return setNodeValid.contains(p.getNode());
    }

    public boolean canSwitchOrAdd(DocumentInProcess p) {
        if (p == null || p.getDocument() == null)
            return false;
        DocumentStatusEnum docSts = p.getDocument().getStatus();
        List<DocumentStatusEnum> status = Arrays.asList(DocumentStatusEnum.RETURN_DOC, DocumentStatusEnum.RETAKE_DOC,
                DocumentStatusEnum.DONE);
        return !status.contains(docSts) && !isStatus(p.getHandleStatus(), CHUYEN_DON_VI)
                && !isStatus(p.getHandleStatus(), DO_NOT_RELATED)
                && !isStatus(p.getHandleStatus(), HANDLE_STATUS_DOING); //CR 25/3/2022 Khi chuyển xử lý xong mới được thêm xử lý
    }

    public boolean canOrgTransfer(DocumentInProcess p, List<NodeDto> nodes, boolean review, boolean isDoneDoc) {
        if (p == null || review || isDoneDoc)
            return false;
        boolean allowMultiple = nodes.stream().filter(i -> i.isAllowMultiple()).count() > 0;
        return allowMultiple && NOT_YET_ALL.contains(p.getHandleStatus()) && passReview(p);
    }

    /**
     * Check node has close branch
     *
     * @param nodes
     * @return
     */
    public boolean isCloseBranchNodes(List<NodeDto> nodes) {
        if (BussinessCommon.isEmptyList(nodes))
            return false;

        for (NodeDto i : nodes) {
            if (Boolean.TRUE.equals(i.getCloseBranch()))
                return true;
        }

        return false;
    }

    public boolean canDone(DocumentInProcess p, boolean review, boolean lastNode, boolean isDoneDoc,
                           Boolean mergedLines, Boolean isCloseBranchNodes) {
        // #Ticket 2803 : Cho phép TK trưởng phòng hoàn thành vb đến nội bộ khi vừa nhận
        // được
        if (Boolean.TRUE.equals(mergedLines) && p != null && !isDoneDoc)
            return true;

        // #Ticket 2750 : Close brach => Đánh dấu record này đã được đóng theo nhánh nên không cho hoàn thành nữa
        if (p == null || Boolean.TRUE.equals(review) || Boolean.TRUE.equals(p.getCloseBranch())
                || p.getStep() == Constant.START_STEP || Boolean.TRUE.equals(p.getEndTask()))
            return false;

        if (!passReview(p)) return false;

        // Chuyển đơn vị thì không cho Hoàn thành xử lý
        // Đơn vị con Hoàn thành chuyển -> trạng thái process thành Đã xử lý thì mới được hoàn thành xử lý
        if (DocumentInHandleStatusEnum.CHUYEN_DON_VI.equals(p.getHandleStatus())) {
            return false;
        }

        // #Ticket 2165
        // Document is not yet -> can done
        // Document is done and user status handle not yet -> can done
        // #Ticket 2750 : Khi chuyển xử lý , vai trò xử lý chính hoặc phối hợp đều có thể hoàn thành xử lý.
        // Vai trò phối hợp chỉ có thể hoàn thành khi đã chuyển xử lý (transfer = true)
        boolean isNotYetHandle = isStatus(p.getHandleStatus(), HANDLE_STATUS_NOT_YET);
        boolean isDoneHandle = isStatus(p.getHandleStatus(), DONE);
        //checked status node mark done by branch
        // 14/4/2022 : Văn bản ở tab Đã xử lý cho phép hoàn thành xử lý để qua tab Hoàn thành

        if (Boolean.TRUE.equals(isPreviousLastNode)) {
            boolean isPreviousNode = nodeRepo.isPreviousNextNode(p.getId());
            if (Boolean.TRUE.equals(isPreviousNode)) {
                List<Long> listPosition = nodeRepo.getListPositionInConditionByNodeId(p.getNode());
                if (listPosition != null) {
                    return listPosition.contains(BussinessCommon.getUser().getPosition());
                }
            }
        }

        if ((Boolean.TRUE.equals(isCloseBranchNodes) && isDoneHandle && Boolean.TRUE.equals(p.getTransfer())
                && !isDoneDoc) || (isNotYetHandle) || (!isDoneDoc && isDoneHandle && p.getEndTask() == null))
            return true;

        // check last node
        return lastNode && !isDoneHandle;
    }

    public boolean canTransfer(DocumentInProcess p, List<NodeDto> nodes, Boolean review) {
        if (p == null || p.getDocument() == null || Boolean.TRUE.equals(review))
            return false;
        boolean notYetStatusHandle = isStatus(p.getHandleStatus(), HANDLE_STATUS_NOT_YET);
        boolean transferDocStatus = !isStatus(p.getDocument().getStatus(), DOC_STATUS_NOT_TRANSFER);
        boolean hasNextNode = bpmnService.hasNextNode(nodes);
        return hasNextNode && transferDocStatus && notYetStatusHandle && passReview(p);
    }

    public boolean canReturn(DocumentInProcess p, boolean isDoneDoc) {
        if (p == null || p.getStep().equals(Constant.START_STEP) || isDoneDoc
//				|| transferToUser(p.getDocId()).isEmpty()
        )
            return false;

        return isStatus(p.getHandleStatus(), HANDLE_STATUS_RETURN);
    }

    public Long countDocByUser(List<DocumentInHandleStatusEnum> handleStatus, List<DocumentStatusEnum> docStatus) {
        User u = BussinessCommon.getUser();
        return processRepository.countDocByUser(u.getClientId(), u.getId(), handleStatus, docStatus);
    }

    public Long countDocByHandleTypeAndHandleStatusAndDocStatus(HandleTypeEnum handleType,
                                                                List<DocumentInHandleStatusEnum> handleStatus, List<DocumentStatusEnum> docStatus) {
        User u = BussinessCommon.getUser();
        return processRepository.countDocByUser(u.getClientId(), u.getId(), handleType, handleStatus, docStatus);
    }

    public Long countDocByUserAndOverDue(List<DocumentInHandleStatusEnum> handleStatus,
                                         List<DocumentStatusEnum> docStatus) {
        User u = BussinessCommon.getUser();
        return processRepository.countDocByUserAndOverDue(u.getClientId(), u.getId(), handleStatus, docStatus);
    }

    public List<DocumentInProcess> findByDocIdAndLastedStep(List<Long> docIdList) {
        return processRepository.findByDocIdAndLastedStep(BussinessCommon.getClientId(), docIdList);
    }

    public List<DocumentInProcess> findByDocIdAndLastedStep(List<Long> docIdList, HandleTypeEnum type) {
        return processRepository.findByDocIdAndLastedStep(BussinessCommon.getClientId(), docIdList, type);
    }

    public DocumentInProcess saveFrDelegate(DocumentInProcess olds, HandleTypeEnum type,
                                            DocumentInHandleStatusEnum status) {
        User u = BussinessCommon.getUser();
        DocumentInProcess o = findByDocIdAndStepAndHandleTypeAndToUser(olds.getDocId(), olds.getStep(), u.getId(),
                type);
        DocumentInProcess news = o == null ? new DocumentInProcess() : o;
        news.setHandleStatus(status);
        news.setHandleType(type);
        news.setToUser(u.getId());
        news.setFrUser(olds.getFrUser());
        news.setNode(olds.getNode());
        news.setDocId(olds.getDocId());
        news.setStep(olds.getStep());
        news.setOrgName(u.getOrgModel().getName());
        news.setDelegaterId(olds.getToUser());
        news.setProgress(100);
        return processRepository.save(news);
    }

    public DocumentInProcess saveFrDelegate2(DocumentInProcess olds, HandleTypeEnum type,
                                             DocumentInHandleStatusEnum status) {
        User u = BussinessCommon.getUser();
        DocumentInProcess news = new DocumentInProcess();
        news.setHandleStatus(status);
        news.setHandleType(type);
        news.setToUser(u.getId());
        news.setFrUser(olds.getFrUser());
        news.setNode(olds.getNode());
        news.setPreNode(olds.getPreNode());
        news.setDocId(olds.getDocId());
        news.setStep(olds.getStep());
        news.setOrgName(u.getOrgModel().getName());
        if (DocumentInHandleStatusEnum.DA_XU_LY_UQ.equals(status))
            news.setProgress(100);
        return processRepository.save(news);
    }

    public DocumentInProcess progressReport(Long docId, Integer progress, String comment, String tab) {
        BussinessCommon.validLengthData(comment, "Nội dung báo cáo", 200);
        HandleTypeEnum type = null;
        User user = BussinessCommon.getUser();
        DocumentInProcess oProcess;
        // for delegate
        if (tab != null) {
            if ("xu_ly_chinh".equals(tab)) {
                type = HandleTypeEnum.MAIN;
            }
            if ("xu_ly_phoi_hop".equals(tab)) {
                type = HandleTypeEnum.SUPPORT;
            }
            oProcess = findByDelegateAndDocId(user.getId(), docId, type);
        } else {
            oProcess = findByToUserAndDocId2(user.getId(), docId);
        }

        if (progress == null || progress.intValue() < 0 || progress.intValue() > 100) {
            throw new RestExceptionHandler(Message.UPDATE_PROCESS);
        }
        oProcess.setProgress(progress);
        oProcess.setComment(comment);
        return processRepository.save(oProcess);
    }

    public DocumentInProcess findLastStepByDocIdAndHandleType(Long docId, HandleTypeEnum handleType) {
        return processRepository.findFirstByDocIdAndHandleTypeAndActiveOrderByStepDesc(docId, handleType, true);
    }

    public DocumentInProcess findByStepAndDocIdAndHandleTypeAndReview(Long docId, int step, HandleTypeEnum handleType,
                                                                      boolean review) {
        return processRepository.findByDocIdAndStepAndHandleTypeAndReviewAndActive(docId, step, handleType, review,
                true);
    }

    public List<Long> findByRelatedAndDocId(List<Long> userId, Long docId) {
        try {
            return processRepository.findByRelatedAndDocId(userId, docId, BussinessCommon.getClientId());
        } catch (Exception e) {
            e.printStackTrace();
        }
        return new ArrayList<>();
    }

    public DocumentInProcess findByDelegateAndDocIdAndHandleStatus(Long delegateId, Long docId,
                                                                   HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus) {
        return processRepository
                .findFistByDelegaterIdAndDocIdAndHandleTypeAndHandleStatusAndActiveAndClientIdOrderByIdDesc(delegateId,
                        docId, handleType, handleStatus, true, BussinessCommon.getClientId());
    }

    public DocumentInProcess findByToUserAndDocIdAndHandleStatus(Long userId, Long docId, HandleTypeEnum handleType,
                                                                 DocumentInHandleStatusEnum handleStatus) {
        return processRepository.findFistByToUserAndDocIdAndHandleTypeAndHandleStatusAndActiveAndClientIdOrderByIdDesc(
                userId, docId, handleType, handleStatus, true, BussinessCommon.getClientId());
    }

    public List<DocumentInProcess> getTransferStep(Long docId) {
        return processRepository.getTransferStep(docId, HandleTypeEnum.MAIN, DONE, BussinessCommon.getClientId());
    }

    public DocumentInProcess transferToUserId(Long docId, Long userId) {
        List<DocumentInProcess> pList = processRepository.transferToUserId(docId, userId,
                BussinessCommon.getClientId());
        if (pList.isEmpty())
            return null;
        return pList.get(0);
    }

    public Integer getMaxTransferStep(Long docId) {
        return processRepository.getMaxTransferStep(docId, DONE, BussinessCommon.getClientId());
    }

    public List<DocumentInProcess> save(List<DocumentInProcess> oldProcess, Integer step, Integer maxTransferStep) {
        List<DocumentInProcess> rsList = new ArrayList<>();
        if (oldProcess.isEmpty())
            return rsList;

        oldProcess.forEach(i -> {
            DocumentInProcess news = new DocumentInProcess();
            news.set(i, step, maxTransferStep);
            rsList.add(news);
        });

        return getRepository().saveAll(rsList);
    }

    public List<DocumentInProcess> updateTransferStep(List<DocumentInProcess> oldProcess, Integer transferStep) {
        if (oldProcess.isEmpty())
            return new ArrayList<>();
        oldProcess.forEach(i -> i.setTransferStep(transferStep));
        return getRepository().saveAll(oldProcess);
    }

    public boolean canAsk(Long docId, Long userId) {
        try {
            DocumentInProcess oProcess = findByToUserAndDocId(userId, docId);
            if (oProcess == null)
                return false;

            Documents doc = oProcess.getDocument();
            boolean isDoneDoc = false;
            if (doc != null && DocumentStatusEnum.DONE.equals(doc.getStatus())) {
                isDoneDoc = true;
            }

            return canAsk(oProcess, isDoneDoc);
        } catch (Exception e) {
            return false;
        }
    }

    public DocumentInConditionDto checkCondition(Long docId, Long userId) {
        return new DocumentInConditionDto(canAsk(docId, userId));
    }

    public ReportDocByTypeDto reportDocByType() {
        User user = BussinessCommon.getUser();
        List<ReportDocByTypeDto> rsList = processRepository.reportDocByType(clericalOrg, user.getId(), user.getOrg(),
                user.getClientId());
        return new ReportDocByTypeDto().count(rsList, this.docInternalRemove);
    }

    public ListObjectDto<DocumentInProcess> checkTypeHandleByDoc(Long docId) {
        return BussinessCommon.convert(processRepository.findAllByToUserAndDocId(docId, BussinessCommon.getUserId(),
                BussinessCommon.getClientId()));
    }

    public DocumentInProcess findByToUserAndDocIdAndNode(Long node, Long userId, Long docId) {
        List<DocumentInProcess> rs = processRepository.findByToUserAndDocIdAndNode(node, docId, userId,
                BussinessCommon.getClientId());
        return rs.isEmpty() ? null : rs.get(0);
    }

    public DocumentInProcess findByDocIdAndNodeAndType(Long docId, Long node, HandleTypeEnum type) {
        List<DocumentInProcess> rs = processRepository.findByDocIdAndNodeAndType(docId, node, type,
                BussinessCommon.getClientId());
        return rs.isEmpty() ? null : rs.get(0);
    }

    public Boolean finishReceiveToKnow(List<Long> docIds, String comment) {
        Long userId = BussinessCommon.getUserId();
        List<DocumentInProcess> oProcesses = new ArrayList<>();
        for (Long docId : docIds) {
            DocumentInProcess oProcess = findByToUserAndDocId2(userId, docId);
            if (oProcess == null)
                throw new RestExceptionHandler(Message.INVALID_PROCESS);
            oProcesses.add(oProcess);
        }
        List<DocumentComment> comments = new ArrayList<>();
        for (DocumentInProcess oProcess : oProcesses) {
            oProcess.setHandleStatus(DocumentInHandleStatusEnum.DA_XU_LY);

            noticeService.setActiveForUserHandle(userId, oProcess.getDocId(), DocumentTypeEnum.VAN_BAN_DEN, false,
                    NotificationHandleStatusEnum.NHAN_DE_BIET, ModuleCodeEnum.DOC_OUT_KNOW);
            if (!StringUtils.isNullOrEmpty(comment)) {
                DocumentComment cmt = new DocumentComment();
                cmt.setDocId(oProcess.getDocId());
                cmt.setComment(comment);
                comments.add(cmt);
                cmtService.saveCmt(cmt);
            }
        }

        for (DocumentComment cmt : comments) {
            cmt.setUserPosition(BussinessCommon.getUser().getPositionModel().getName());
            cmt.setUserFullName(BussinessCommon.getUser().getFullName());
        }
        commentRepository.saveAll(comments);

        return true;
    }

    public List<UserConditionDto> getUserByNodeId(Long docId, Long nodeId, int step) {
        List<UserConditionDto> rs = new ArrayList();
        List<DocumentInProcess> pList = processRepository.getProcesByNodeIdAndStep(docId, nodeId, step,
                BussinessCommon.getClientId());
        for (DocumentInProcess i : pList) {
            if (DocumentInHandleStatusEnum.DA_TRA_LAI.equals(i.getHandleStatus())
                    || DocumentInHandleStatusEnum.DA_XU_LY_SWITCH.equals(i.getHandleStatus())) {
                continue;
            }

            if (i.getDelegaterId() != null) {
                rs.add(UserConditionDto.set(i, true));
            }
            rs.add(UserConditionDto.set(i, false));
        }
        return rs;
    }

    public List<UserConditionDto> getUserByNodeId(Long docId, Long nodeId) {
        return processRepository.getUserByNodeId(docId, nodeId, BussinessCommon.getClientId());
    }

    public Set<HandleTypeEnum> getHandleTypeByDocId(Long userId, Long docId) {
        return processRepository.getHandleTypeByDocId(userId, docId, BussinessCommon.getClientId());
    }

    public List<DocumentInProcess> findProcessByToUserAndDocId(Long userId, Long docId) {
        return processRepository.findByToUserAndDocIdAndClientIdAndActive(userId, docId, BussinessCommon.getClientId(),
                true);
    }

    public List<DocumentInProcess> getListTransferTo(Long userId, Long docId) {
        return processRepository.getListTransferTo(userId, docId, BussinessCommon.getClientId());
    }

    public List<Long> getListToUserId(Long userId, Long docId) {
        return processRepository.getListToUserId(userId, docId, BussinessCommon.getClientId());
    }

    public List<Long> getListToUserIdByListFromUser(List<Long> listFrUser, Long docId) {
        return processRepository.getListToUserIdByListFromUser(listFrUser, docId, BussinessCommon.getClientId());
    }

    public List<DocumentInProcess> getListToTransfer(Long userId, Long docId) {
        return processRepository.getListToTransfer(userId, docId, BussinessCommon.getClientId());
    }

    public List<Long> getListFromUserId(Long userId, Long docId) {
        return processRepository.getListFromUserId(userId, docId, BussinessCommon.getClientId());
    }

    public List<Long> findToUserByFromUserAndDocIdAndStep(Long fromUser, Long docId, Integer step) {
        return processRepository.findToUserByFrUserAndDocIdAndStepAndClientIdAndActive(fromUser, docId, step,
                BussinessCommon.getClientId(), true);
    }

    public List<Long> findToUserByFromUserAndDocIdAndStepAndHandleTypeIn(Long fromUser, Long docId, Integer step,
                                                                         List<HandleTypeEnum> handleType) {
        return processRepository.findToUserByFrUserAndDocIdAndStepAndHandleTypeInAndClientIdAndActive(fromUser, docId,
                step, handleType, BussinessCommon.getClientId(), true);
    }

    public List<User> findToUserObjByFromUserAndDocIdAndStepAndHandleTypeIn(Long fromUser, Long docId, Integer step,
                                                                            List<HandleTypeEnum> handleType) {
        return processRepository.findToUserObjByFrUserAndDocIdAndStepAndHandleTypeInAndClientIdAndActive(fromUser,
                docId, step, handleType, BussinessCommon.getClientId(), true);
    }

    public List<Long> findToUserByFromUserAndDocIdAndStepAndHandleTypeInAndLeadership(Long fromUser, Long docId,
                                                                                      Integer step, List<HandleTypeEnum> handleType, Boolean leadership) {
        return processRepository.findToUserByFrUserAndDocIdAndStepAndHandleTypeInAndLeadershipAndClientIdAndActive(
                fromUser, docId, step, handleType, leadership, BussinessCommon.getClientId(), true);
    }

    public List<Long> findToUserByFromUserAndDocIdAndStepAndOrgId(Long fromUser, Long docId, Integer step, Long orgId) {
        return processRepository.findToUserByFrUserAndDocIdAndStepAndOrgIdAndClientIdAndActive(fromUser, docId, step,
                orgId, BussinessCommon.getClientId(), true);
    }

    public List<Long> findToUserByFromUserAndDocIdAndOrgId(Long frUser, Long docId, Long orgId) {
        return processRepository.findToUserByFrUserAndDocIdAndOrgIdAndClientIdAndActive(frUser, docId, orgId,
                BussinessCommon.getClientId(), true);
    }

    public List<Long> findToUserByFromUserAndDocId(Long userId, Long docId) {
        return processRepository.findToUserByFromUserAndDocIdAndClientIdAndActive(userId, docId,
                BussinessCommon.getClientId(), true);
    }

    public List<Long> findToUserByFromUserInAndDocIdAndStepGreaterAndLeadership(List<Long> fromUsers, Long docId,
                                                                                Integer step, boolean leadership) {
        return processRepository.findToUserByFromUserInAndDocIdAndStepGreaterAndLeadershipAndClientIdAndActive(
                fromUsers, docId, step, leadership, BussinessCommon.getClientId(), true);
    }

    public List<Long> findToUserByDocIdAndAuthority(Long docId, boolean isLeadership) {
        return processRepository.findToUserByDocIdAndAuthorityAndClientIdAndActive(docId, isLeadership,
                BussinessCommon.getClientId(), true);
    }

    public ReportDocByTypeDto reportDocDelegate() {
        Long userId = BussinessCommon.getUserId();
        Long clientId = BussinessCommon.getClientId();
        List<DocumentStatusEnum> docInStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
        List<DocumentStatusEnum> docOutStatus = Arrays.asList(DocumentStatusEnum.DU_THAO, DocumentStatusEnum.BI_TRA_LAI,
                DocumentStatusEnum.THU_HOI_XL, DocumentStatusEnum.DANG_XU_LY, DocumentStatusEnum.CHO_BAN_HANH,
                DocumentStatusEnum.DA_BAN_HANH);
        List<DocumentInHandleStatusEnum> docInHandleStatus = DocumentInHandleStatusEnum.getHandleStatusByName(
                "CHO_XU_LY", "DANG_XU_LY", "CHUYEN_DON_VI", "CHO_DANH_GIA", "XIN_DANH_GIA", "DG_CHAP_NHAN",
                "DG_TU_CHOI");
        List<HandleTypeEnum> handleType = HandleTypeEnum.getListHandleType(HandleTypeEnum.SUPPORT, HandleTypeEnum.MAIN);
        List<DocumentOutHandleStatusEnum> docOutHandleStatus = Arrays
                .asList(DocumentOutProcessController.getStatus("waiting-handle"));

        ReportDocByTypeDto docIn = docRepository.reportDocDelegate(userId, docInStatus, docInHandleStatus, handleType,
                clientId);
        ReportDocByTypeDto docOut = delegateRepository.reportDocDelegate(userId, new Date(), docOutHandleStatus,
                docOutStatus, clientId);
        List<ReportDocByTypeDto> dtos = new ArrayList<>();
        dtos.add(docIn);
        dtos.add(docOut);
        return new ReportDocByTypeDto(dtos);
    }

    public List<KPIDataDto> findAllByToUser(List<Long> userIds, Date startDate, Date endDate) {
        return processRepository.findAllByToUser(userIds, BussinessCommon.getClientId(), startDate, endDate);
    }

    public List<KPIDataDto> findAllByToUser(Long userId, Date startDate, Date endDate) {
        return processRepository.findAllByToUser(userId, BussinessCommon.getClientId(), startDate, endDate);
    }

    public List<TrackingDto> buildTree(Long docId) {
        List<DocumentInProcess> allList = getListProcessByDocId(docId);
        List<TrackingDto> tList = TrackingDto.getInstance().castToList(allList, readToDoing);
        List<TrackingDto> parents = getParents(tList);
        pushTree(parents, tList);
        return parents;
    }

    private void pushTree(List<TrackingDto> parents, List<TrackingDto> childs) {
        if (parents.isEmpty() || childs.isEmpty())
            return;
        for (TrackingDto p : parents) {
            setSubList(p, childs);
            pushTree(p.getChildren(), childs);
        }
    }

    private void setSubList(TrackingDto p, List<TrackingDto> childs) {
        if (childs.isEmpty())
            return;
        for (TrackingDto i : childs) {
            if (p.getToUser().equals(i.getFrUser()) && (Math.abs(i.getStep() - p.getStep()) == 1)
                    && (!i.getToUser().equals(i.getFrUser()))) {
                p.getChildren().add(i);
            }
        }
        childs.removeIf(i -> p.getChildren().contains(i));
    }

    private List<TrackingDto> getParents(List<TrackingDto> all) {
        Map<Long, List<TrackingDto>> entry = all.stream().filter(i -> i.getStep() == Constant.START_STEP)
                .collect(Collectors.groupingBy(TrackingDto::getFrUser));

        List<TrackingDto> rs = new ArrayList<>();
        entry.forEach((k, v) -> rs.addAll(v));
        return rs;
    }

    public DocumentInProcess findByDocIdAndUserIdAndLessStep(Long docId, Long userId, int step) {
        List<DocumentInProcess> rs = processRepository.findByDocIdAndUserIdAndLessStep(docId, userId, step,
                BussinessCommon.getClientId());
        return rs.isEmpty() ? null : rs.get(0);
    }

    public List<DocumentInProcess> findByDocIdAndGreaterStep(Long docId, int step) {
        return processRepository.findByDocIdAndGreaterStep(docId, step, BussinessCommon.getClientId());
    }

    /**
     * Get process transfer to currents user
     *
     * @param docId
     * @return
     */
    public List<DocumentOutProcessDto> transferToUser(Long docId) {
        Long cUserId = BussinessCommon.getUserId();
        DocumentInProcess old = findByToUserAndDocId2(cUserId, docId);
        if (old == null)
            return new ArrayList<>();

        DocumentInProcess pointReturn = findByDocIdAndUserIdAndLessStep(docId, old.getFrUser(), old.getStep());
        List<DocumentInProcess> rs = new ArrayList<>();
        if (pointReturn != null) {
            rs.add(pointReturn);
        }
        return DocumentOutProcessDto.convert(rs, false);
    }

    public List<DocumentOutProcessDto> userTransferTo(Long docId) {
        DocumentInProcess old = findByToUserAndDocId2(BussinessCommon.getUserId(), docId);
        List<DocumentInProcess> list = userTransferTo(old, docId);
        return DocumentOutProcessDto.convert(list, true);
    }

    /**
     * Get process transfer to user
     *
     * @param docId
     * @return
     */
    public List<DocumentInProcess> userTransferTo(DocumentInProcess old, Long docId) {
        List<DocumentInProcess> pList = findByDocIdAndGreaterStep(docId, old.getStep());
        return getParentChilds(Arrays.asList(old), pList, new ArrayList<>());
    }

    private List<DocumentInProcess> getParentChilds(List<DocumentInProcess> parents, List<DocumentInProcess> all,
                                                    List<DocumentInProcess> rs) {
        if (parents.isEmpty())
            return rs;
        for (DocumentInProcess p : parents) {
            getChilds(p, all, rs);
            getParentChilds(p.getChilds(), all, rs);
        }
        return rs;
    }

    private void getChilds(DocumentInProcess p, List<DocumentInProcess> all, List<DocumentInProcess> rs) {
        rs.add(p);
        if (all.isEmpty())
            return;
        for (DocumentInProcess i : all) {
            if (p.getToUser().equals(i.getFrUser())) {
                p.getChilds().add(i);
            }
        }

        all.removeIf(i -> p.getChilds().contains(i));
    }

    public void inactive(List<DocumentInProcess> pList) {
        pList.forEach(i -> i.setActive(false));
        processRepository.saveAll(pList);
    }

    public List<DocumentInProcess> findByDocIdAndStepAndFrUser(Long docId, Integer step, Long userId) {
        return processRepository.findByDocIdAndStepAndFrUser(docId, step, userId, BussinessCommon.getClientId());
    }

    /**
     * all users returned, will update again the status of people transfer process
     * (frUser)
     *
     * @param old
     */
    public void reOpenProcess(DocumentInProcess old) {
        List<DocumentInProcess> tmpList = findByDocIdAndStepAndFrUser(old.getDocId(), old.getStep() + 1,
                old.getToUser());
        Optional<DocumentInProcess> tmp = tmpList.stream()
                .filter(i -> DocumentInHandleStatusEnum.DA_TRA_LAI.equals(i.getHandleStatus())
                        && HandleTypeEnum.MAIN.equals(i.getHandleType()))
                .findFirst();
        if (tmp.isPresent() || Constant.START_STEP.equals(old.getStep())) {
            old.setHandleStatus(DocumentInHandleStatusEnum.CHO_XU_LY);
        }
    }

    public Map<Long, DocumentInProcess> getLastStepByDocId(List<Long> docIds) {
        List<DocumentInProcess> pList = processRepository.getLastStepByDocId(docIds, BussinessCommon.getClientId());
        Map<Long, DocumentInProcess> map = new HashMap<>();
        for (Long docId : docIds) {
            for (DocumentInProcess p : pList) {
                if (p.getDocId().equals(docId)) {
                    map.put(docId, p);
                }
            }
        }
        return map;
    }

    /**
     * Get all user related document
     *
     * @param docId
     * @return
     */
    public List<Long> findByRelatedAndDocId(Long docId) {
        List<Long> docIds = new ArrayList<>();
        docIds.add(docId);

        Documents doc = docRepository.findByClientIdAndId(BussinessCommon.getClientId(), docId);
        if (doc == null) {
            return Collections.emptyList();
        }

        // Org transfer
        List<Documents> childs = doc.getListChildren();
        if (!BussinessCommon.isEmptyList(childs)) {
            docIds.addAll(childs.stream().map(Documents::getId).collect(Collectors.toList()));
        }

        // User transfer + delegate
        List<DocumentInProcess> pList = processRepository.findByRelatedAndDocId(docIds, BussinessCommon.getClientId());
        List<Long> all = new ArrayList<>();
        for (DocumentInProcess p : pList) {
            all.add(p.getToUser());
            if (p.getDelegaterId() != null) {
                all.add(p.getDelegaterId());
            }
        }

        // Opinion
        List<Long> opnUserIds = manipulationService.findRelatedByDocId(docIds);
        all.addAll(opnUserIds);

        return all.stream().distinct().collect(Collectors.toList());
    }

    public List<Documents> validDocIdList(List<Long> input) {
        List<Long> distinct = input.stream().distinct().collect(Collectors.toList());
        List<Documents> docList = docRepository.findByDocIdList(distinct, BussinessCommon.getClientId());
        if (docList.size() != distinct.size()) {
            throw new RestExceptionHandler(Message.NOT_FOUND_DOC);
        }
        return docList;
    }

    private List<DocumentInProcess> findByToUserAndDocId2(Long userId, List<Long> docIds) {
        return processRepository.findByToUserAndDocId2(userId, docIds, BussinessCommon.getClientId());
    }

    /**
     * #2803 : Hoàn thành đối với văn bản đến nội bộ
     *
     * @param docIds
     * @param comment
     * @param files
     * @return
     */
    @Transactional
    public Boolean done(List<Long> docIds, String comment, MultipartFile[] files) {
        validDocIdList(docIds);
        List<DocumentInProcess> pList = findByToUserAndDocId2(BussinessCommon.getUserId(), docIds);
        List<Long> tmp = pList.stream().map(DocumentInProcess::getDocId).distinct().collect(Collectors.toList());
        if (tmp.size() != docIds.size())
            throw new RestExceptionHandler(Message.NOT_FOUND_DOC);
        pList.forEach(i -> handleDoneDocument(i.getDocId(), i, comment, files));
        return true;
    }

    /**
     * Tìm người mà đã chuyển văn bản đến đơn vị con
     *
     * @param docId
     * @param orgTransfer
     * @return
     */
    public DocumentInProcess getTransferParentDoc(Long docId, Long orgTransfer) {
        List<DocumentInProcess> pList = processRepository.getTransferParentDoc(docId, orgTransfer,
                BussinessCommon.getClientId());
        return pList.isEmpty() ? null : pList.get(0);
    }
}