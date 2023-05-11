package com.vz.backend.business.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.vz.backend.business.config.DocumentCommentTypeEnum;
import com.vz.backend.business.config.ViewStatusEnum;
import com.vz.backend.business.domain.*;
import com.vz.backend.business.domain.documentInternal.DocInternalAttach;
import com.vz.backend.business.domain.documentInternal.DocInternalComment;
import com.vz.backend.business.dto.*;
import com.vz.backend.business.dto.document.DocInternalAttachDto;
import com.vz.backend.business.dto.kpi.KPIDataDto;
import com.vz.backend.business.dto.report.ReportData;
import com.vz.backend.business.repository.IDocumentRepository;
import com.vz.backend.business.repository.IKeywordSearchRepository;
import com.vz.backend.business.repository.ITaskRepository;
import com.vz.backend.business.service.docInternal.DocInternalAttachService;
import com.vz.backend.business.service.docInternal.DocInternalCommentService;
import com.vz.backend.business.service.docInternal.DocInternalService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.*;
import com.vz.backend.core.domain.Module;
import com.vz.backend.core.domain.*;
import com.vz.backend.core.dto.LabelValueId;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.repository.IUserRepository;
import com.vz.backend.core.service.*;
import com.vz.backend.util.StringUtils;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.nio.file.Paths;
import java.util.*;
import java.util.stream.Collectors;

@Service
public class CommonService {

    @Value("${configs.clerical-org: false}")
    private boolean clericalOrg;

    @Value("${configs.doc-in.return-previous-node: false}")
    private boolean returnPreviousNode;

    @Autowired
    private UserService userService;

    @Autowired
    private OrganizationService orgService;

    @Autowired
    private CategoryService categoryService;

    @Autowired
    private DocumentService docInService;

    @Autowired
    private ITaskRepository taskRepository;

    @Autowired
    private DocumentInProcessService docInProcessService;

    @Autowired
    private DocumentOutProcessService docOutProcessService;

    @Autowired
    private TaskService taskService;

    @Autowired
    private DocumentInManipulationService docInManipulationService;

    @Autowired
    private AttachmentCalendarService calAttService;

    @Autowired
    private ModuleService moduleService;

    @Autowired
    private IKeywordSearchRepository keySearchRepo;

    @Autowired
    private EncryptionService encryptService;

    @Autowired
    private FilesStorageService storageService;

    @Autowired
    private AttachmentService docInAttService;

    @Autowired
    private AttachmentCommentService docInCmtAttService;

    @Autowired
    private DocumentOutAttachmentService docOutAttService;

    @Autowired
    private TaskAttachmentService taskAttService;

    @Autowired
    private DocInternalAttachService docInterAttService;

    @Autowired
    private ClericalOrgService clericalOrgService;

    @Autowired
    private DocumentCommentService docInCmtService;

    @Autowired
    private DocumentOutCommentService docOutCmtService;

    @Autowired
    private TaskExecuteService taskExeService;

    @Autowired
    private DocInternalService docInternalService;

    @Autowired
    private Calendar2Service calendar2Service;

    @Autowired
    private TaskCommentService taskCommentService;

    @Autowired
    private DocInternalCommentService docInternalCmtService;

    @Autowired
    private ObjectReadService objectReadService;

    @Autowired
    private IUserRepository userRepository;

    @Autowired
    private DocumentInTrackingService documentInTrackingService;

    @Autowired
    private NotificationService notificationService;
    @Autowired
    private IDocumentRepository documentRepository;

    @Autowired
    private DocumentUserService documentUserService;
    @Autowired
    private AuthorityUserService authorityService;

    public List<ResultQuickSearchDto> quickSearchAllObject(String text) {
        User u = BussinessCommon.getUser();
        text = BussinessCommon.convert(text);
        boolean isLibrarianDocIn = userService.isVanThuVBDen(u);
        boolean isLibrarianDocOut = userService.isVanThuVBDi(u);
        List<Long> userIds = docInService.getListUserId(isLibrarianDocIn);

        // for calendar
        Long orgTemp = u.getOrg();
        List<Long> orgIdTemps = new ArrayList<>();
        Long orgTopLv;
        List<Long> orgIdTopLvs = new ArrayList<>();
        List<Long> orgIdUnitLvs = new ArrayList<>();

        // top level tab
        if (!orgService.isUserOfOrgType(u, Constant.BAN)) {
            orgTemp = orgService.getParentByOrgType(u.getOrgModel(), Constant.BAN);
        }
        orgIdTemps = orgService.orgAndSub(orgTemp);
        if (!orgIdTemps.isEmpty() && orgIdTemps.contains(orgTemp)) {
            orgIdTemps.remove(orgTemp);
        }
        orgTopLv = orgTemp;
        orgIdTopLvs = orgIdTemps;

        // unit level tab
        orgTemp = u.getOrg();
        if (orgService.isUserOfOrgType(u, Constant.BAN)) {
            orgIdUnitLvs = new ArrayList<>();
        } else {
            if (!orgService.isUserOfOrgType(u, Constant.CUC_VU_VIEN)) {
                orgTemp = orgService.getParentByOrgType(u.getOrgModel(), Constant.CUC_VU_VIEN);
            }
            orgIdTemps = orgService.orgAndSub(orgTemp);
            orgIdUnitLvs = orgIdTemps;
        }
        return taskRepository.quickSearchAllObject(new Date(), clericalOrg, u.getId(), isLibrarianDocIn, userIds, text,
                isLibrarianDocOut, u.getOrg(), u.isLead(), orgTopLv, orgIdTopLvs, orgIdUnitLvs, u.getClientId());
    }

    public ListObjectDto<ResultQuickSearchDto> quickSearchAllObject(int page, String text) {
        saveKeySearch(text);
        List<ResultQuickSearchDto> rs = quickSearchAllObject(text).stream().distinct().collect(Collectors.toList());
        return BussinessCommon.getListByPageNumber(rs, page);
    }

    public ReportData statistic(Date startDate, Date endDate) {
        User u = BussinessCommon.getUser();

        List<KPIDataDto> docInList = docInProcessService.findAllByToUser(u.getId(), startDate, endDate);
        List<KPIDataDto> docOutList = docOutProcessService.findAllByToUser(u.getId(), startDate, endDate);
        List<KPIDataDto> taskList = taskService.findAllByToUser(u.getId(), startDate, endDate);
        List<Long> outShowDone = docOutProcessService.knowable(u.getId(), startDate, endDate, true);
        List<Long> outShowNotYet = docOutProcessService.knowable(u.getId(), startDate, endDate, false);


//		long inWaitComment = docInManipulationService.waitComment(u.getId(), startDate, endDate);

        ReportData dto = new ReportData(userService.isVanThuVBDen(u), returnPreviousNode);
        dto.set(docInList, DocumentTypeEnum.VAN_BAN_DEN);
        dto.set(docOutList, DocumentTypeEnum.VAN_BAN_DI);
        dto.set(taskList, DocumentTypeEnum.GIAO_VIEC);
//		dto.setInWaitComment(inWaitComment);
        dto.setOutShow(outShowDone, outShowNotYet);
        return dto;
    }

    public Object StactisticsNotComplete() {
        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();
        Pageable pageable = BussinessCommon.castToPageable(1, 10);
        ListObjectDto<DocumentDto> listVbdcxl = docInService.findProcessingDoc(null, 0, null, null, 0, null, null, null,
                null, null, null, pageable);
        ListObjectDto<DocumentDto> listVbdph = docInService.findProcessingDoc(null, 1, null, null, 0, null, null, null,
                null, null, null, pageable);
        ListObjectDto<DocumentDto> listVbdndb = docInService.findProcessingDoc(null, 2, null, null, 0, null, null, null,
                null, null, null, pageable);
        List<Task> listtaskAll = taskService.listTaskStactistics();
        List<Task> listtaskXLc = new ArrayList<>();
        List<Task> listtaskPh = new ArrayList<>();
        for (Task task : listtaskAll
        ) {
            List<TaskExecute> cout = taskExeService.sizetaskPh(task.getId());
            if (cout.size() == 0) {
                listtaskXLc.add(task);
            } else {
                listtaskPh.add(task);
            }
        }
        objectNode.put("vbd_xlc_cht", listVbdcxl.getTotalRecord());
        objectNode.put("vbd_ph_cht", listVbdph.getTotalRecord());
        objectNode.put("vbd_ph_ndb", listVbdndb.getTotalRecord());
        objectNode.put("task_xlc", listtaskXLc.size());
        objectNode.put("task_ph", listtaskPh.size());
        return objectNode;
    }

    public List<LabelValueId<String>> usage() {
        List<LabelValueId<String>> rs = new ArrayList<>();
        List<Module> modules = moduleService.findByParentCode(Constant.USER_MANUAL_CODE);
        List<AttachmentCalendar> aList = calAttService.getByObjId(null, ObjTypeEnum.MODULE);
        for (Module i : modules) {
            Optional<AttachmentCalendar> a = aList.stream().filter(j -> i.getId().equals(j.getObjId())).findFirst();
            if (a.isPresent()) {
                rs.add(new LabelValueId<>(i.getId(), i.getName(), a.get().getDisplayName()));
            }
        }

        return rs;
    }

    public Boolean delUsageModule(Long id) {
        moduleService.delUsageModule(id);
        calAttService.delByIdCat(id);
        return true;
    }

    public Module editUsageModule(Long id, String name, MultipartFile file) {
        Module module = moduleService.editUsageModule(id, name);
        if (file != null) {
            calAttService.updateByIdCat(id, file);
        }

        return module;
    }

    public List<String> getKeySearch() {
        User user = BussinessCommon.getUser();
        return keySearchRepo.getKeySearch(user.getId(), user.getClientId());
    }

    public void saveKeySearch(String text) {
        if (text == null || text.isEmpty())
            return;
        User user = BussinessCommon.getUser();
        KeywordSearch old = keySearchRepo.findByKeyAndUserIdAndClientIdAndActiveTrue(text, user.getId(),
                user.getClientId());
        if (old == null) {
            keySearchRepo.save(new KeywordSearch(text));
        }
    }

    private String tmpEncFileName = null;

    public Boolean encrypt(String key, MultipartFile encrypted, Long objId, DocumentTypeEnum type) {
        if (encrypted == null || StringUtils.isNullOrEmpty(key))
            return false;
        try {
            String encryptName = storageService.save(encrypted, Paths.get(Constant.ENCRYPT_FILE_PATH));
            this.tmpEncFileName = encryptName;
//			List<Long> usersEncrypt = getUserOrListVanThuBan();
//			for (Long userId: usersEncrypt) {
            encryptService.save(new Encryption(key, encryptName, BussinessCommon.getUserId()));
//			}
            saveAttObj(objId, type, encryptName, encrypted);
            return true;
        } catch (Exception e) {
            e.printStackTrace();
            return false;
        }
    }

    public EncryptDto encryptAndFileName(String key, MultipartFile encrypted, Long objId, DocumentTypeEnum type) {
        EncryptDto res = new EncryptDto();

        if (encrypted == null || StringUtils.isNullOrEmpty(key)) {
            res.setResult(false);
        }
        try {
            String encryptName = storageService.save(encrypted, Paths.get(Constant.ENCRYPT_FILE_PATH));
            this.tmpEncFileName = encryptName;
//			List<Long> usersEncrypt = getUserOrListVanThuBan();
//			for (Long userId: usersEncrypt) {
            encryptService.save(new Encryption(key, encryptName, BussinessCommon.getUserId()));
//			}
            saveAttObj(objId, type, encryptName, encrypted);
            res.setResult(true);
            res.setFileName(encryptName);
            List<Long> userIds = new ArrayList<>();
            userIds.add(BussinessCommon.getUserId());
            Category positionVanThuBan = categoryService.findByName(BussinessCommon.getClientId(), Constant.VAN_THU_MAIN);
            if (BussinessCommon.getUser().getPositionModel().getId().longValue() == positionVanThuBan.getId().longValue()) {
                userIds = getUserOrListVanThuBan();
            }
            res.setUserIds(userIds);
            return res;
        } catch (Exception e) {
            e.printStackTrace();
            res.setResult(false);
            return res;
        }
    }

    public void saveAttObj(Long objId, DocumentTypeEnum type, String encryptName, MultipartFile encrypted) {
        switch (type) {
            case VAN_BAN_DEN:
                docInAttService.add(encrypted, objId, encryptName);
                break;
            case VAN_BAN_DEN_CMT:
                docInCmtAttService.add(encrypted, objId, encryptName);
                break;
            case VAN_BAN_DI_DU_THAO:
                docOutAttService.add(encrypted, objId, encryptName, AttachmentTypeEnum.DRAFT);
                break;
            case VAN_BAN_DI_LIEN_QUAN:
                docOutAttService.add(encrypted, objId, encryptName, AttachmentTypeEnum.RELATE);
                break;
            case VAN_BAN_DI_BINH_LUAN: //to do
                docOutAttService.add(encrypted, objId, encryptName, AttachmentTypeEnum.COMMENT);
                break;
            case GIAO_VIEC:
                taskAttService.add(encrypted, objId, encryptName, 1L);
                break;
            case GIAO_VIEC_BINH_LUAN:
                taskAttService.add(encrypted, objId, encryptName, 2L);
                break;
            case TAO_LICH:
                calAttService.add(encrypted, objId, encryptName, ObjTypeEnum.CALENDAR);
                break;
            case VAN_BAN_SOAN_THAO:
                calAttService.add(encrypted, objId, encryptName, ObjTypeEnum.WORD_EDITOR);
                break;
            case VAN_BAN_NOI_BO_VAN_BAN:
                docInterAttService.add(encrypted, objId, encryptName, AttachmentTypeEnum.VAN_BAN);
                break;
            case VAN_BAN_NOI_BO_PHU_LUC:
                docInterAttService.add(encrypted, objId, encryptName, AttachmentTypeEnum.PHU_LUC);
                break;
            case VAN_BAN_NOI_BO_BINH_LUAN:
                docInterAttService.add(encrypted, objId, encryptName, AttachmentTypeEnum.BINH_LUAN);
                break;
            default:
                break;
        }
    }

    /**
     * Lấy xác thực token tương ứng theo từng userId hoặc theo đơn vị
     *
     * @param userIds
     * @param orgIds
     * @param type
     * @return
     */
    public List<LabelValueId<String>> getCertByUserId(Long[] userIds, Long[] orgIds, String type, Boolean skipError) {
        List<LabelValueId<String>> rs = new ArrayList<>();
        List<LabelValueId<String>> frUserIds = new ArrayList<>();
        List<LabelValueId<String>> frOrgIds = new ArrayList<>();

        if (!ArrayUtils.isEmpty(userIds)) {
            frUserIds = userService.getCertAndFullNameByUserId(Arrays.asList(userIds), null);
        }

        if (!ArrayUtils.isEmpty(orgIds)) {
            List<Long> tmpUserIds = getListUserByOrgIdVBDIBANHANH(orgIds, type);
            frOrgIds = userService.getCertAndFullNameByUserId(tmpUserIds, null);
        }

        rs.addAll(frUserIds);
        rs.addAll(frOrgIds);

        if (Boolean.TRUE.equals(skipError)) {
            return rs;
        }

        userService.validCertByUserId(rs);
        return rs;
    }

    /**
     * Lấy xác thực token tương ứng theo từng userId hoặc theo đơn vị, chỉ check trường phòng
     *
     * @param userIds
     * @param orgIds
     * @param type
     * @return
     */
    public List<LabelValueId<String>> getCertByUserIdManager(Long[] userIds, Long[] orgIds, String type, Boolean skipError) {
        List<LabelValueId<String>> rs = new ArrayList<>();
        List<LabelValueId<String>> frUserIds = new ArrayList<>();
        List<LabelValueId<String>> frOrgIds = new ArrayList<>();

        if (!ArrayUtils.isEmpty(userIds)) {
            frUserIds = userService.getCertAndFullNameByUserIdManager(Arrays.asList(userIds), null);
        }

        if (!ArrayUtils.isEmpty(orgIds)) {
            List<Long> tmpUserIds = getListUserByOrgIdVBDIBANHANH(orgIds, type);
            frOrgIds = userService.getCertAndFullNameByUserIdManager(tmpUserIds, null);
        }

        rs.addAll(frUserIds);
        rs.addAll(frOrgIds);

        if (Boolean.TRUE.equals(skipError)) {
            return rs;
        }

        userService.validCertByUserId(rs);
        return rs;
    }

    List<Long> removePCVP(List<Long> users) {
        Long idPCVP = categoryService.findByName("Phó Chánh Văn phòng").getId();
        for (int i = 0; i < users.size(); i++) {
            User user = userService.getOne(users.get(i));
            if (user.getPosition() == idPCVP) {
                users.remove(i);
                return users;
            }
        }
        return users;
    }

    /**
     * Lấy userId theo từng loại văn bản liên quan tới tổ chức
     *
     * @param orgIds
     * @param type
     * @return
     */
    private List<Long> getListUserByOrgId(Long[] orgIds, String type) {
        List<Long> userIds = new ArrayList<>();

        switch (type) {
            case Constant.TASK:
            case Constant.DOC_IN_TRANSFER:
                userIds = removePCVP(userService.getLeadBasicByOrgs(orgIds));
                break;

            case Constant.DOC_IN_ORG_TRANSFER:
                List<Long> tmpOrgTranfer;
                for (Long i : orgIds) {
                    tmpOrgTranfer = clericalOrg ? clericalOrgService.getClericalOrgByOrgId(i)
                            : userService.getListIdsVanThuVBDenByOrg(i);

                    userIds.addAll(tmpOrgTranfer);
                }
                break;
            case Constant.DOC_OUT_INTERNAL: // Chuyển nơi nhận nội bộ
                // TODO: chưa lấy văn thư/ tổ chức mà khi thông luồng
                List<Long> tmpInternal = userService.getListLeadUserIdByOrg(Arrays.asList(orgIds));
                userIds.addAll(tmpInternal);
                break;

            case Constant.DOC_INTERNAL: // lấy theo lãnh đạo của tổ chức
                List<Long> tmpDocInInternal = userService.findLeadershipByOrgIdIn(Arrays.asList(orgIds));
                userIds.addAll(tmpDocInInternal);
                break;
            default:
                throw new RestExceptionHandler(Message.ENCRYPTED_FILE_INVALID);
        }

        return userIds;
    }

    private List<Long> getListUserByOrgIdVBDIBANHANH(Long[] orgIds, String type) {
        List<Long> userIds = new ArrayList<>();

        switch (type) {
            case Constant.TASK:
            case Constant.DOC_IN_TRANSFER:
                userIds = userService.getLeadBasicByOrgs(orgIds);
                break;

            case Constant.DOC_IN_ORG_TRANSFER:
                List<Long> tmpOrgTranfer;
                for (Long i : orgIds) {
                    tmpOrgTranfer = clericalOrg ? clericalOrgService.getClericalOrgByOrgId(i)
                            : userService.getListIdsVanThuVBDenByOrg(i);

                    userIds.addAll(tmpOrgTranfer);
                }
                break;
            case Constant.DOC_OUT_INTERNAL: // Chuyển nơi nhận nội bộ
                // TODO: chưa lấy văn thư/ tổ chức mà khi thông luồng
                for (Long i : orgIds) {
                    tmpOrgTranfer = clericalOrg ? clericalOrgService.getClericalOrgByOrgId(i)
                            : userService.getListIdsVanThuVBDenByOrg(i);

                    userIds.addAll(tmpOrgTranfer);
                }
                break;

            case Constant.DOC_INTERNAL: // lấy theo lãnh đạo của tổ chức
                List<Long> tmpDocInInternal = userService.findLeadershipByOrgIdIn(Arrays.asList(orgIds));
                userIds.addAll(tmpDocInInternal);
                break;
            default:
                throw new RestExceptionHandler(Message.ENCRYPTED_FILE_INVALID);
        }

        return userIds;
    }

    public List<LabelValueId<String>> getCertByObjId(Long objId, String type) {
        List<Long> tmp = new ArrayList();
        switch (type) {
            case Constant.DOC_IN_RETURN:
                // Chia sẻ file đính kèm cho danh sách người ở node trước
                List<DocumentInProcess> previousProcessList = docInProcessService.getTransferStep(objId);
                tmp = previousProcessList.stream().map(DocumentInProcess::getToUser).collect(Collectors.toList());
                break;
            case Constant.DOC_IN_DONE:
                // áp dụng cho hoàn thành/ thu hồi hoàn thành/ thu hồi
                // Chia sẻ file đính kèm cho toàn bộ danh sách người có trong văn bản
                tmp = docInProcessService.findByRelatedAndDocId(objId);
                break;
            case Constant.DOC_OUT_COMMENT:
                tmp.add(BussinessCommon.getUserId());
            case Constant.DOC_OUT_DONE:
                // áp dụng cho hoàn thành/ thu hồi hoàn thành/ thu hồi
                // Chia sẻ file đính kèm cho toàn bộ danh sách người có trong văn bản
                tmp.addAll(docOutProcessService.getAllRelateByDocId(objId, BussinessCommon.getClientId()));
                break;
            case Constant.DOC_IN_COMMENT:
                // Chia sẻ file đính kèm cho người xin ý kiến
                tmp = docInManipulationService.findFromUserByToUserAndDocId(BussinessCommon.getUserId(), objId);
                tmp.addAll(docInProcessService.findByRelatedAndDocId(objId));
                tmp.add(BussinessCommon.getUserId());
                break;
            case Constant.TASK:
                // Chia sẻ file đính kèm cho người thực hiện
                tmp = taskExeService.findUserIdByTaskId(objId);
                break;
            case Constant.TASK_COMMENT:
                // Chia sẻ file đính kèm cho người thực hiện
                tmp = taskExeService.findUserIdByTaskId(objId);
                tmp.add(BussinessCommon.getUserId());
                break;
            case Constant.DOC_INTERNAL_COMMENT:
                tmp = docInternalService.getRelatedByDocId(objId);
                tmp.add(BussinessCommon.getUserId());
                break;
            default:
                break;
        }
        return userService.getCertAndFullNameByUserId(tmp, null);
    }

    public List<LabelValueId<String>> getCertByObjIdEdit(Long objId, String type) {
        List<Long> tmp = new ArrayList();
        switch (type) {
            case Constant.DOC_IN_RETURN:
                // Chia sẻ file đính kèm cho danh sách người ở node trước
                List<DocumentInProcess> previousProcessList = docInProcessService.getTransferStep(objId);
                tmp = previousProcessList.stream().map(DocumentInProcess::getToUser).collect(Collectors.toList());
                break;
            case Constant.DOC_IN_DONE:
                // áp dụng cho hoàn thành/ thu hồi hoàn thành/ thu hồi
                // Chia sẻ file đính kèm cho toàn bộ danh sách người có trong văn bản
                tmp = docInProcessService.findByRelatedAndDocId(objId);
                break;
            case Constant.DOC_OUT_COMMENT:
                tmp.add(BussinessCommon.getUserId());
            case Constant.DOC_OUT_DONE:
                // áp dụng cho hoàn thành/ thu hồi hoàn thành/ thu hồi
                // Chia sẻ file đính kèm cho toàn bộ danh sách người có trong văn bản
                tmp.addAll(docOutProcessService.getAllRelateByDocIdEdit(objId, BussinessCommon.getClientId()));
                break;
            case Constant.DOC_IN_COMMENT:
                // Chia sẻ file đính kèm cho người xin ý kiến
                tmp = docInManipulationService.findFromUserByToUserAndDocId(BussinessCommon.getUserId(), objId);
                tmp.addAll(docInProcessService.findByRelatedAndDocId(objId));
                tmp.add(BussinessCommon.getUserId());
                break;
            case Constant.TASK:
                // Chia sẻ file đính kèm cho người thực hiện
                tmp = taskExeService.findUserIdByTaskId(objId);
                break;
            case Constant.TASK_COMMENT:
                // Chia sẻ file đính kèm cho người thực hiện
                tmp = taskExeService.findUserIdByTaskId(objId);
                tmp.add(BussinessCommon.getUserId());
                break;
            case Constant.DOC_INTERNAL_COMMENT:
                tmp = docInternalService.getRelatedByDocId(objId);
                tmp.add(BussinessCommon.getUserId());
                break;
            default:
                break;
        }
        return userService.getCertAndFullNameByUserId(tmp, null);
    }

    public List<LabelValueId<String>> getUserShareFile(Long objId, String type) {
        User user = BussinessCommon.getUser();
        List<Long> userIds = new ArrayList<>();
        switch (type) {
            case Constant.DOC_IN_ADD:
                // Danh sách tiếp nhận : Văn thư cùng đơn vị
                userIds = clericalOrg ? clericalOrgService.getClericalOrgByOrgId(user.getOrg())
                        : userService.getListIdsVanThuVBDenByOrg(user.getOrg());
                break;
            case Constant.DOC_IN_COMMENT:
                // Danh sách bình luận : Người được chuyển xử lý tới/người ủy quyền/ chuyển đơn vị/
                userIds = docInProcessService.findByRelatedAndDocId(objId);
                break;
            case Constant.DOC_OUT_ADD: // Danh sách cùng đơn vị
                userIds = userService.findUserIdByOrgId(user.getId());
                break;
            case Constant.DOC_OUT_COMMENT:
                // Danh sách bình luận : Người được chuyển xử lý tới/Người kí/Văn thư
                //TODO: Chuyển nơi nhận nội bộ
                userIds = docOutProcessService.getAllRelateByDocId(objId, user.getClientId());
                break;
            case Constant.TASK:
                // Danh sách bình luận : Người được chuyển xử lý tới
                userIds = taskExeService.findUserIdByTaskId(objId);
                break;
            case Constant.DOC_INTERNAL:
                // Danh sách bình luận : Người được chuyển xử lý tới
                userIds = docInternalService.getRelatedByDocId(objId);
                break;
            case Constant.CALENDAR:
                // Danh sách Người tham dự
                userIds = calendar2Service.getListMemberByCalId(objId);
                userIds.addAll(calendar2Service.getListMemberInOrgByCalendarId(objId));
                userIds.addAll(calendar2Service.getListMemberInGroupByCalendarId(objId));
                break;
            default:
                break;
        }
        userIds.addAll(userService.findUserIdByOrgId(user.getOrg()));
        return userService.getCertAndFullNameByUserId(userIds, false);
    }

    /**
     * Lưu nội dung bình luận và đính kèm theo loại
     *
     * @param <T>
     * @param objId
     * @param type
     * @param comment
     * @param files
     * @param encFileNameKeys : danh sách bao gồm tên file mã hóa và key tương ứng
     * @return danh sách tên file đã mã hóa (nếu có)
     */
    public <T> FileNameDto saveCommentByType(Long objId, DocumentTypeEnum type, String comment, String cmtContent,
                                             MultipartFile[] nonEncfiles, MultipartFile[] encFiles, String[] keys, DocumentCommentTypeEnum cmtType) {
        try {
            Long cmtId = saveObjByType(objId, type, comment, cmtContent, cmtType);
            if (cmtId == null) {
                return new FileNameDto();
            }
            if (type.equals(DocumentTypeEnum.VAN_BAN_DEN)) {
                Documents doc = docInService.validDocId(objId);
                if (doc != null) {
                    List<DocumentInTrackingEnum> actions = new ArrayList<>();
                    actions.add(DocumentInTrackingEnum.INCOMING);
                    actions.add(DocumentInTrackingEnum.FINISH);
                    List<Long> userIds = documentInTrackingService.getUserIdByDocIdAndActions(objId, actions); // ID user đã xử lý và mới đến văn bản
                    for (Long userId : userIds) {
                        notificationService.add(userId, objId, doc.getPreview(), type, NotificationHandleStatusEnum.DA_Y_KIEN, getModuleCodeEnumByType(type));
                    }
                } else {
                    throw new RestExceptionHandler(Message.ENCRYPTED_FILE_INVALID);
                }
            }
            List<String> nonEncFileNames = saveNonEncFile(cmtId, type, nonEncfiles);
            List<String> encFileNames = saveEncFile(cmtId, type, encFiles, keys);
            return new FileNameDto(encFileNames, nonEncFileNames, cmtId);
        } catch (Exception e) {
            e.printStackTrace();
            throw new RestExceptionHandler(e.getMessage());
        }
    }

    private ModuleCodeEnum getModuleCodeEnumByType(DocumentTypeEnum type) {
        if (type.equals(DocumentTypeEnum.VAN_BAN_DI)) {
            return ModuleCodeEnum.DOCUMENT_IN;
        }
        return ModuleCodeEnum.DOC_OUT_MAIN;
    }

    /**
     * Lưu tệp đính kèm (có mã hóa) của đối tượng theo loại
     *
     * @param objId
     * @param type
     * @param comment
     * @param files
     * @param keys
     * @return
     */
    private List<String> saveEncFile(Long objId, DocumentTypeEnum type, MultipartFile[] files,
                                     String[] keys) {
        List<String> rsList = new ArrayList<>();
        if (BussinessCommon.isEmptyArr(files) || BussinessCommon.isEmptyArr(keys) || files.length != keys.length
                || objId == null)
            return rsList;

        MultipartFile f;
        for (int i = 0; i < files.length; i++) {
            f = files[i];
            switch (type) {
                case VAN_BAN_DEN_CMT:
                case VAN_BAN_DI_BINH_LUAN:
                case GIAO_VIEC_BINH_LUAN:
                case VAN_BAN_NOI_BO_BINH_LUAN:
                    encrypt(keys[i], f, objId, type);
                    break;
                default:
                    break;
            }

            if (this.tmpEncFileName != null) {
                rsList.add(this.tmpEncFileName);
                this.tmpEncFileName = null;
            }
        }

        return rsList;
    }

    /**
     * Lưu tệp đính kèm (không mã hóa) của đối tượng theo loại
     *
     * @param <T>
     * @param objId   Comment Id
     * @param type
     * @param comment
     * @param files
     */
    private <T> List<String> saveNonEncFile(Long objId, DocumentTypeEnum type, MultipartFile[] files) {
        if (BussinessCommon.isEmptyArr(files) || objId == null)
            return Collections.emptyList();

        switch (type) {
            case VAN_BAN_DEN_CMT:
                List<AttachmentComment> docInAtmCmt = docInCmtAttService.addListAttachmentComment(files, objId);
                return docInAtmCmt.stream().map(AttachmentComment::getName).collect(Collectors.toList());

            case VAN_BAN_DI_BINH_LUAN:
                List<DocumentOutAttachment> docOutAtmCmt = docOutAttService.addListAttachment(files,
                        AttachmentTypeEnum.COMMENT.getName(), objId);
                return docOutAtmCmt.stream().map(DocumentOutAttachment::getName).collect(Collectors.toList());

            case GIAO_VIEC_BINH_LUAN:
                List<TaskAttachment> taskAtmCmt = taskAttService.addListAttachment(files, objId, 2L);
                return taskAtmCmt.stream().map(TaskAttachment::getName).collect(Collectors.toList());

            case VAN_BAN_NOI_BO_BINH_LUAN:
                List<DocInternalAttach> docInternalAtmCmt = docInterAttService.addListAttachmentComment(files, objId, true);
                return docInternalAtmCmt.stream().map(DocInternalAttach::getName).collect(Collectors.toList());

            default:
                break;
        }

        return Collections.emptyList();
    }

    /**
     * Lưu nội dung đối tượng
     *
     * @param <T>
     * @param objId
     * @param type
     * @param comment
     * @return
     */
    private Long saveObjByType(Long objId, DocumentTypeEnum type, String comment, String cmtContent, DocumentCommentTypeEnum cmtType) {

        switch (type) {
            case VAN_BAN_DEN_CMT:
                if (comment == null) {
                    comment = " ";
                }
                DocumentComment docInCmt = docInCmtService.save(comment, cmtContent, objId, cmtType);
                return docInCmt == null ? null : docInCmt.getId();
            case VAN_BAN_DI_BINH_LUAN:
                DocumentOutComment docOutCmt = docOutCmtService.saveCmt(objId, comment);
                return docOutCmt == null ? null : docOutCmt.getId();
            case GIAO_VIEC_BINH_LUAN:
                TaskComment taskCmt = taskCommentService.save(new TaskComment(comment, objId));
                return taskCmt == null ? null : taskCmt.getId();
            case VAN_BAN_NOI_BO_BINH_LUAN:
                DocInternalComment docInternalCmt = docInternalCmtService.save(comment, objId);
                return docInternalCmt == null ? null : docInternalCmt.getId();
            default:
                break;
        }
        return null;
    }

    /**
     * Xóa nội dung và đính kèm của đối tượng theo loại
     *
     * @param fileNames
     * @param type
     * @return
     */
    public Boolean delCommentByType(String[] fileNames, Long[] userIds, DocumentTypeEnum type, Long cmtIdSaved) {
        switch (type) {
            case VAN_BAN_DEN_CMT:
                docInCmtAttService.delByAttachAndComment(fileNames, cmtIdSaved);
                encryptService.delEncrypt(fileNames, userIds);
                break;
            case VAN_BAN_DI_BINH_LUAN:
                docOutCmtService.deleteComment(cmtIdSaved, false);
                encryptService.delEncrypt(fileNames, userIds);
                break;
            case GIAO_VIEC_BINH_LUAN:
            case GIAO_VIEC:
                taskCommentService.deleteComment(cmtIdSaved, false);
                encryptService.delEncrypt(fileNames, userIds);
                break;
            case VAN_BAN_NOI_BO_BINH_LUAN:
                docInternalCmtService.deleteComment(cmtIdSaved, false);
                encryptService.delEncrypt(fileNames, userIds);
                break;
            default:
                break;
        }

        return true;
    }

    /**
     * Return file names encrypted
     *
     * @param objId
     * @param type
     * @return
     */
    public List<String> getAttachsByTypeAndObjId(Long objId, String type) {
        List<String> rs = new ArrayList<>();

        switch (type) {
            case Constant.DOC_IN_ADD:
                List<Attachment> docInAttachs = docInAttService.findByObjId(objId);
                if (!BussinessCommon.isEmptyList(docInAttachs)) {
                    rs = docInAttachs.stream().filter(i -> Boolean.TRUE.equals(i.getEncrypt())).map(Attachment::getName)
                            .collect(Collectors.toList());
                }
                break;
            case Constant.DOC_OUT_INTERNAL:
            case Constant.DOC_OUT_ADD:
                List<DocumentOutAttachment> docOutAttachs = docOutAttService.getListAttachment(objId);
                if (!BussinessCommon.isEmptyList(docOutAttachs)) {
                    rs = docOutAttachs.stream().filter(i -> Boolean.TRUE.equals(i.getEncrypt())).map(DocumentOutAttachment::getName)
                            .collect(Collectors.toList());
                }
                break;
            case Constant.TASK:
                List<TaskAttachment> tasks = taskAttService.findByObjId(objId, 1L, BussinessCommon.getClientId(), true);
                if (!BussinessCommon.isEmptyList(tasks)) {
                    rs = tasks.stream().filter(i -> Boolean.TRUE.equals(i.getEncrypt())).map(TaskAttachment::getName)
                            .collect(Collectors.toList());
                }
                break;
            case Constant.DOC_INTERNAL:
                List<DocInternalAttachDto> docInternals = docInterAttService.getListAttachByDocId(objId);
                if (!BussinessCommon.isEmptyList(docInternals)) {
                    rs = docInternals.stream().filter(i -> Boolean.TRUE.equals(i.getEncrypt()))
                            .map(DocInternalAttachDto::getName).collect(Collectors.toList());
                }
                break;
            case Constant.CALENDAR:
                List<AttachmentCalendar> aList = calAttService.getByObjId(objId, ObjTypeEnum.CALENDAR);
                if (!BussinessCommon.isEmptyList(aList)) {
                    rs = aList.stream().filter(i -> Boolean.TRUE.equals(i.getEncrypt()))
                            .map(AttachmentCalendar::getName).collect(Collectors.toList());
                }
                break;
            default:
                break;
        }

        return rs;
    }

    public ObjectNode getUserIdLeadBan() {
        ObjectMapper objectMapper = new ObjectMapper();
        ObjectNode objectNode = objectMapper.createObjectNode();
        User currentUser = BussinessCommon.getUser();
        List<String> userLeadNames = new ArrayList<>();
        List<String> userVanThu = new ArrayList<>();
        if (Constant.CHANH_VAN_PHONG.equals(currentUser.getPositionModel().getName().toLowerCase())) {
            userLeadNames.add(Constant.POSITION_MAIN);
            List<Long> userLanhDaoBanIds = userRepository.getUserLeadOrgBan(userLeadNames, orgService.getRootOrgId(currentUser.getOrg()));
            objectNode.put("truongBanId", userLanhDaoBanIds.size() > 0 ? userLanhDaoBanIds.get(0) : null);
            userVanThu.add("văn thư đơn vị");
            List<Long> orgIds = orgService.orgAndSub(currentUser.getOrg());
            List<Long> vanThu = userRepository.getUserByPositionAndOrg(userVanThu, orgIds);
            objectNode.putPOJO("vanThuDonViId", vanThu);
            objectNode.put("isTransfer", true);
            objectNode.put("comment", "Văn thư đơn vị: " + (vanThu != null ? userService.findByUserId(vanThu.get(0)).getFullName() : ""));
            objectNode.put("commentTruongBan", "Trưởng ban: " + (userLanhDaoBanIds != null ? userService.findByUserId(userLanhDaoBanIds.get(0)).getFullName() : ""));


        } else if (Constant.POSITION_MAIN.equals(currentUser.getPositionModel().getName().toLowerCase())) {
            userLeadNames.add(Constant.VAN_THU_MAIN);
            List<Long> orgIds = orgService.orgAndSub(orgService.getRootOrgId(currentUser.getOrg()));
            List<Long> userBanThuBan = userRepository.getUsersByNameCategory(Constant.VAN_THU_MAIN, orgIds);
            objectNode.put("truongBanId", userBanThuBan.get(0));
            objectNode.put("isTransfer", true);
            objectNode.put("comment", userBanThuBan.size() > 1 ? "Văn thư ban xử lý" : "văn thư ban: " + userService.getFullNameById(userBanThuBan.get(0)));
        } else if ("cục trưởng".equals(currentUser.getPositionModel().getName().toLowerCase()) || "vụ trưởng".equals(currentUser.getPositionModel().getName().toLowerCase()) || "giám đốc".equals(currentUser.getPositionModel().getName().toLowerCase())) {
            userVanThu.add("văn thư đơn vị");
            List<Long> orgIds = orgService.orgAndSub(currentUser.getOrg());
            orgIds.remove(currentUser.getOrg());
            List<Long> vanThu = userRepository.getUserByPositionAndOrg(userVanThu, orgIds);
            objectNode.putPOJO("vanThuDonViId", vanThu);
            objectNode.put("isTransfer", true);
            objectNode.put("comment", "văn thư đơn vị: " + vanThu != null ? userService.findByUserId(vanThu.get(0)).getFullName() : "");
        } else {
            objectNode.put("isTransfer", false);
        }
        return objectNode;
    }

    public List<Long> getUserSharedFile(String fileNames) {
        List<Encryption> eList = encryptService.getByName(fileNames);
        return eList.stream().map(Encryption::getUserId).distinct().collect(Collectors.toList());
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

    public Page<com.vz.backend.business.dto.document.DocumentDto> documentsByViewStatus(Long orgId, ViewStatusEnum viewStatus, HandleTypeEnum handleType, Date startDate, Date endDate, Pageable pageable) {
        checkUserPermission(AuthorityEnum.VIEW_STATISTIC_BY_DOC_HANDLE_STATE);
        Page<com.vz.backend.business.dto.document.DocumentDto> rs;
        Long clientId = BussinessCommon.getClientId();
        orgId = BussinessCommon.getValueOrDefault(orgId, BussinessCommon.getOrgId());
        handleType = BussinessCommon.getValueOrDefault(handleType, HandleTypeEnum.MAIN);

        rs = documentRepository.pageDocumentDtoByHandleState(String.valueOf(viewStatus), handleType, orgId, new Date(), startDate, endDate, clientId, pageable);
        return rs;
    }

    public ReportByViewStatus statisticByViewStatus(Long orgId, HandleTypeEnum handleType, Date startDate, Date endDate) {
        checkUserPermission(AuthorityEnum.VIEW_STATISTIC_BY_DOC_HANDLE_STATE);
        ReportByViewStatus reportByViewStatus = new ReportByViewStatus();

        Long clientId = BussinessCommon.getClientId();

        orgId = BussinessCommon.getValueOrDefault(orgId, BussinessCommon.getOrgId());
        handleType = BussinessCommon.getValueOrDefault(handleType, HandleTypeEnum.MAIN);

        reportByViewStatus.setOrgId(orgId);
        Organization orgById = orgService.getById(reportByViewStatus.getOrgId());
        List<Organization> childOrgs = orgService.getCurrentOrgWholeTree(orgById);
        reportByViewStatus.setOrgName(orgById.getName());
        reportByViewStatus.setChildOrganizations(childOrgs);
        Map<ViewStatusEnum, Long> documentsCountByViewStatus = new HashMap<>();

        reportByViewStatus.setChartName(String.format("Thống kê văn bản theo trạng thái xử lý của: %s", reportByViewStatus.getOrgName()));
        Long orgUsersCount = orgService.getUserCount(reportByViewStatus.getOrgId(), true);
        reportByViewStatus.setOrgUsersCount(orgUsersCount);

        Date currentDate = new Date();

        List<com.vz.backend.business.dto.document.DocumentDto> listDocumentDtos = documentRepository.getDocumentHandleStateByOrgId(handleType,
                reportByViewStatus.getOrgId(), currentDate, startDate, endDate, clientId);

        for (com.vz.backend.business.dto.document.DocumentDto d : listDocumentDtos) {
            if (d.getViewStatus() != null) {
                if (documentsCountByViewStatus.containsKey(d.getViewStatus())) {
                    Long currentCount = documentsCountByViewStatus.get(d.getViewStatus());
                    documentsCountByViewStatus.put(d.getViewStatus(), currentCount + 1);
                } else {
                    documentsCountByViewStatus.put(d.getViewStatus(), 1L);
                }
            }
        }

        reportByViewStatus.setDocumentsCountByViewStatus(documentsCountByViewStatus);
        reportByViewStatus.setViewStatusDescription(ViewStatusEnum.getDescriptions());
        return reportByViewStatus;
    }

    private void checkUserPermission(AuthorityEnum authorityEnum) {
        boolean hasPermission = authorityService.isUserHasAuthority(BussinessCommon.getUserId(), null, authorityEnum);
        if (!hasPermission) {
            throw new RestForbidden(String.format("Tài khoản hiện tại không có quyền %s", authorityEnum.getName()));
        }
    }

    public void createDocumentUserAndObjectRead(User clerical, Documents document, Long clientId) {
        Date now = new Date();

        DocumentUser documentUser = new DocumentUser();
        documentUser.setDocId(document.getId());
        documentUser.setUserId(clerical.getId());
        documentUser.setDocType(DocumentTypeEnum.VAN_BAN_DEN);
        documentUser.setClientId(clientId);
        documentUser.setCreateBy(clerical.getId());
        documentUser.setCreateDate(now);
        documentUserService.save(documentUser);

        ObjectRead objectRead = new ObjectRead();
        objectRead.setObjId(document.getId());
        objectRead.setUserId(clerical.getId());
        objectRead.setType(DocumentTypeEnum.VAN_BAN_DEN);
        objectRead.setClientId(clientId);
        objectRead.setCreateBy(clerical.getId());
        objectRead.setCreateDate(now);
        objectReadService.save(objectRead);
    }
}
