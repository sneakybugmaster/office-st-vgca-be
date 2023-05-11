package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.EntityManagerFactory;

import com.vz.backend.business.domain.*;
import com.vz.backend.business.dto.*;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.service.FilesStorageService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.databind.node.ObjectNode;
import com.vz.backend.business.config.ButtonStatusEnum;
import com.vz.backend.business.domain.BpmnModel2.TYPE_DOCUMENT;
import com.vz.backend.business.dto.kpi.KPIDataDto;
import com.vz.backend.business.dto.task.TaskButtonActionDto;
import com.vz.backend.business.dto.task.TaskListDto;
import com.vz.backend.business.repository.ITaskDocumentRepository;
import com.vz.backend.business.repository.ITaskExecuteRepository;
import com.vz.backend.business.repository.ITaskHistoryRepository;
import com.vz.backend.business.repository.ITaskRelatedRepository;
import com.vz.backend.business.repository.ITaskRepository;
import com.vz.backend.business.util.Constant;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.UserService;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Service
public class TaskService extends BaseService<Task> {

    @Autowired
    ITaskRepository taskRepository;

    @Autowired
    ITaskExecuteRepository taskExecuteRepository;

    @Autowired
    ITaskDocumentRepository taskDocumentRepository;

    @Autowired
    DocumentOutService docOutService;

    @Autowired
    DocumentService documentService;

    @Autowired
    EntityManagerFactory entityManagerFactory;

    @Autowired
    TaskExecuteService taskExecuteService;

    @Autowired
    OrganizationService orgService;

    @Autowired
    NotificationService noticeService;

    @Autowired
    TaskDocumentService taskDocumentService;

    @Autowired
    WordEditorService weService;

    @Autowired
    ITaskRelatedRepository taskRelatedRepository;

    @Autowired
    TaskCommentService taskCommentService;

    @Autowired
    TaskAttachmentService taskAttService;

    @Autowired
    ReportFieldService rpFieldService;

    @Autowired
    UserService userService;

    @Autowired
    BpmnService2 bpmnService;

    @Autowired
    ITaskHistoryRepository taskHistoryRepository;

    @Autowired
    ObjectReadService objReadService;

    @Autowired
    FilesStorageService filesStorageService;

    @Autowired
    private CommonService commonService;

    @Override
    public IRepository<Task> getRepository() {
        return taskRepository;
    }

    @Value("${configs.task.migrate-thread: false}")
    private boolean migrateByThread;

    private static final Boolean ACTIVE = true;

    public void validatePermission(Task task) {
        if (!checkPermission(task)) {
            throw new RestExceptionHandler(Message.NO_PERMISSION);
        }
    }

    private boolean check(Task task) {
        Long userId = BussinessCommon.getUserId();
        Long orgId = BussinessCommon.getOrgId();
        if (userId.equals(task.getUserAssignId()) || orgId.equals(task.getOrgId())) {
            return true;
        }

        TaskExecute old = taskExecuteService.findByTaskIdAndUserId(task.getId(), userId);
        if (old == null)
            return false;
        return true;
    }

    public boolean checkPermission(Task task) {
        Set<Long> docInChecked = new HashSet<>();
        Set<Long> docOutChecked = new HashSet<>();
        Set<Long> taskChecked = new HashSet<>();
        Set<Long> folderChecked = new HashSet<>();
        return checkPermission(task, docInChecked, docOutChecked, taskChecked, folderChecked);
    }

    public boolean checkPermission(Task task, Set<Long> docInChecked, Set<Long> docOutChecked, Set<Long> taskChecked, Set<Long> folderChecked) {
        if (!taskChecked.contains(task.getId())) {
            taskChecked.add(task.getId());
            if (check(task)) {
                return true;
            }
        }
        //Check văn bản đến và đi liên quan => Công việc giải quyết văn bản.
        if (task.getParentId() != null && checkPermission(task.getParent(), docInChecked, docOutChecked, taskChecked, folderChecked)) {
            return true;
        }
        if (task.getListTaskDocument() != null) {
            for (TaskDocument td : task.getListTaskDocument()) {
                if (Boolean.TRUE.equals(td.getTypeDocument())) {
                    if (!docInChecked.contains(td.getDocId()) && documentService.checkPermission(td.getDocId(), docInChecked, docOutChecked, taskChecked, folderChecked)) {
                        return true;
                    }
                } else {
                    if (!docOutChecked.contains(td.getDocId()) && docOutService.checkPermission(td.getDocId(), docInChecked, docOutChecked, taskChecked, folderChecked)) {
                        return true;
                    }
                }
            }
        }
        //Check văn bản đi giải quyết công việc => Văn bản đi trả lời
        for (DocumentOutTask dt : task.getListDocumentOutTask()) {
            if (!docOutChecked.contains(dt.getDocOutId()) && docOutService.checkPermission(dt.getDocOut(), docInChecked, docOutChecked, taskChecked, folderChecked)) {
                return true;
            }
        }
        return false;
    }

    /**
     * Update status task/ task exe status
     *
     * @param taskId
     * @param status
     * @param assigner : true for assigner tab/false for executor tab
     * @param comment
     * @param files
     * @return
     */
	@Transactional
    public Task updateStatusByCondition(Long taskId, Integer status, Boolean assigner, String comment, MultipartFile[] files) {
        Task task = valid(taskId, Message.TASK_NOT_FOUND);
        if (status == null) { // valid status
            throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
        }
        String newComment = "Từ chối xử lý";
        if (comment != null) {
            newComment = newComment + "\nLý do: " + comment;
        }
        Long userId = BussinessCommon.getUserId();
        boolean isOwner = userId.equals(task.getCreateBy());
        // update task status process
        boolean allDoneIsClose = true; // Constant.TASK_EXCUTE_STATUS_CLOSE == taskExeStatusTmp; Always close all
        boolean isRestore = false;
        if (Boolean.TRUE.equals(assigner)) {
            if (!isOwner) {
                throw new RestExceptionHandler(Message.ACTION_FAILED);
            }

            int taskExeStatusTmp = 0;
            NotificationHandleStatusEnum mainStatusNoti = null;
            NotificationHandleStatusEnum spStatusNoti = null;
            switch (task.getStatus()) {
                case Constant.TASK_STATUS_NEW:
                case Constant.TASK_STATUS_CLOSE:
                case Constant.TASK_STATUS_REVOKE:
                    status = Constant.TASK_STATUS_NEW;
                    isRestore = true;
//				throw new RestExceptionHandler(Message.TASK_STATUS_NO_ACTION);
                    break;
                case Constant.TASK_STATUS_REJECT:
                    commonService.saveCommentByType(task.getId(), DocumentTypeEnum.GIAO_VIEC_BINH_LUAN, newComment, null, files, null, null, null);
                    break;
                case Constant.TASK_STATUS_INPROCESS:
                    switch (status) {
                        case Constant.TASK_STATUS_COMPLETE:
                            status = Constant.TASK_STATUS_CLOSE;
                            taskExeStatusTmp = Constant.TASK_EXCUTE_STATUS_CLOSE;
                            break;
                        case Constant.TASK_STATUS_CLOSE:
                            taskExeStatusTmp = Constant.TASK_EXCUTE_STATUS_CLOSE;
                            break;
                        case Constant.TASK_STATUS_REVOKE:
                            taskExeStatusTmp = Constant.TASK_EXCUTE_STATUS_REVOKE;
                            break;
                        case Constant.TASK_STATUS_REJECT:
                            taskExeStatusTmp = Constant.TASK_EXCUTE_STATUS_INPROCESS;
                            mainStatusNoti = NotificationHandleStatusEnum.CV_XLC_TU_CHOI;
                            spStatusNoti = NotificationHandleStatusEnum.CV_PH_TU_CHOI;
                            allDoneIsClose = false;
                            commonService.saveCommentByType(task.getId(), DocumentTypeEnum.GIAO_VIEC_BINH_LUAN, newComment, null, files, null, null, null);
                            break;
                        default:
//					throw new RestExceptionHandler(Message.TASK_STATUS_NO_ACTION);
                            break;
                    }
                    break;
                case Constant.TASK_STATUS_COMPLETE:
                    switch (status) {
                        case Constant.TASK_STATUS_REVOKE:
                            taskExeStatusTmp = Constant.TASK_EXCUTE_STATUS_REVOKE;
                            break;
                        case Constant.TASK_STATUS_CLOSE:
                            taskExeStatusTmp = Constant.TASK_EXCUTE_STATUS_CLOSE;
                            mainStatusNoti = NotificationHandleStatusEnum.CV_DA_DONG_XU_LY_CHINH;
                            spStatusNoti = NotificationHandleStatusEnum.CV_DA_DONG_PHOI_HOP;
                            break;
                        case Constant.TASK_STATUS_REJECT:
                            taskExeStatusTmp = Constant.TASK_EXCUTE_STATUS_INPROCESS;
                            mainStatusNoti = NotificationHandleStatusEnum.CV_XLC_TU_CHOI;
                            spStatusNoti = NotificationHandleStatusEnum.CV_PH_TU_CHOI;
                            allDoneIsClose = false;
                            commonService.saveCommentByType(task.getId(), DocumentTypeEnum.GIAO_VIEC_BINH_LUAN, newComment, null, files, null, null, null);
                            break;
                        default:
//					throw new RestExceptionHandler(Message.TASK_STATUS_NO_ACTION);
                            break;
                    }
                default:
                    break;
            }

            // update task status
            task.setStatus(status);
            taskRepository.save(task);

            List<TaskExecute> pList = allDoneIsClose ? taskExecuteService.getByTaskIdAndClientId(taskId)
                    : taskExecuteService.getLastStepByTaskId(taskId);
            if (taskExeStatusTmp > 0) {
                updateStatus(pList, taskExeStatusTmp);
            }

            // Restore task-execute
            if (isRestore) {
                this.restoreTask(taskId);
            }

            // notification
            noticeService.setActiveByDocIdAndDocType(task.getId(), DocumentTypeEnum.GIAO_VIEC, false);
            List<Long> userIdSupports = pList.stream().filter(i -> Boolean.TRUE.equals(i.getIsCombination()))
                    .map(TaskExecute::getUserId).collect(Collectors.toList());
            List<Long> userIdMains = pList.stream().filter(i -> Boolean.TRUE.equals(i.getIsExcute()))
                    .map(TaskExecute::getUserId).collect(Collectors.toList());

            if (mainStatusNoti != null) {
                noticeService.addAll(userIdMains, task.getId(), task.getTaskName(), DocumentTypeEnum.GIAO_VIEC,
                        mainStatusNoti, ModuleCodeEnum.TASK_MAIN, task.getUserAssignId());
            }

            if (spStatusNoti != null) {
                noticeService.addAll(userIdSupports, task.getId(), task.getTaskName(), DocumentTypeEnum.GIAO_VIEC,
                        spStatusNoti, ModuleCodeEnum.TASK_SUPPORT, task.getUserAssignId());
            }
        } else {
            TaskExecute old = taskExecuteService.findByTaskIdAndUserId(taskId, userId);
            if (old == null) {
                throw new RestExceptionHandler(Message.ACTION_FAILED);
            }

            NotificationHandleStatusEnum assignStatusNoti = null;
            int taskStatusTmp = 0;
            boolean isMain = Boolean.TRUE.equals(old.getIsExcute());
            boolean commentHandle = false;
            boolean isRevokeFinish = false;
            switch (old.getStatus()) {
                case Constant.TASK_EXCUTE_STATUS_NEW:
                    switch (status) {
                        case Constant.TASK_EXCUTE_STATUS_INPROCESS:
                            assignStatusNoti = NotificationHandleStatusEnum.CV_DA_GIAO_CHAP_NHAN;
                            if (isMain) {
                                taskStatusTmp = Constant.TASK_STATUS_INPROCESS;
                            }
                            break;
                        case Constant.TASK_EXCUTE_STATUS_REJECT:
                            assignStatusNoti = NotificationHandleStatusEnum.CV_DA_GIAO_TU_CHOI;
                            if (isMain) {
                                taskStatusTmp = Constant.TASK_STATUS_REJECT;
                            }
                            commonService.saveCommentByType(task.getId(), DocumentTypeEnum.GIAO_VIEC_BINH_LUAN, newComment, null, files, null, null, null);
                            break;
                        case Constant.TASK_EXCUTE_STATUS_REVOKE:
                        case Constant.TASK_EXCUTE_STATUS_COMPLETE:
                        case Constant.TASK_EXCUTE_STATUS_NEW:
                        default:
                            if (!isOwner) {
                                throw new RestExceptionHandler(Message.TASK_EXE_STATUS_NO_ACTION);
                            }
                    }
                    break;
                case Constant.TASK_EXCUTE_STATUS_INPROCESS:
                    switch (status) {
                        case Constant.TASK_EXCUTE_STATUS_COMPLETE:
                            assignStatusNoti = NotificationHandleStatusEnum.CV_DA_GIAO_HOAN_THANH;
                            if (isMain) {
                                taskStatusTmp = Constant.TASK_STATUS_COMPLETE;
                                old.setClose(true);
                            }
                            commentHandle = true;

                            // If task is closed, close record
                            if (Constant.TASK_STATUS_CLOSE == task.getStatus()) {
                                status = Constant.TASK_EXCUTE_STATUS_CLOSE;
                            }

                            break;
                        case Constant.TASK_EXCUTE_STATUS_INPROCESS:
                        case Constant.TASK_EXCUTE_STATUS_REVOKE:
                        case Constant.TASK_EXCUTE_STATUS_REJECT:
                        case Constant.TASK_EXCUTE_STATUS_NEW:
                        default:
                            if (!isOwner) {
                                throw new RestExceptionHandler(Message.TASK_EXE_STATUS_NO_ACTION);
                            }
                    }
                    break;
                case Constant.TASK_EXCUTE_STATUS_REJECT:
                    switch (status) {
                        case Constant.TASK_EXCUTE_STATUS_INPROCESS:
                            assignStatusNoti = NotificationHandleStatusEnum.CV_DA_GIAO_CHAP_NHAN;
                            if (isMain) {
                                taskStatusTmp = Constant.TASK_STATUS_INPROCESS;
                            }
                            break;
                        case Constant.TASK_EXCUTE_STATUS_COMPLETE:
                        case Constant.TASK_EXCUTE_STATUS_REVOKE:
                        case Constant.TASK_EXCUTE_STATUS_REJECT:
                        case Constant.TASK_EXCUTE_STATUS_NEW:
                        default:
                            throw new RestExceptionHandler(Message.TASK_EXE_STATUS_NO_ACTION);
                    }
                    break;
                case Constant.TASK_EXCUTE_STATUS_COMPLETE: // Revoke Finish
                    status = Constant.TASK_EXCUTE_STATUS_INPROCESS;
                    old.setClose(false);
                    taskStatusTmp = Constant.TASK_STATUS_INPROCESS;
                    isRevokeFinish = true;
                    break;
                case Constant.TASK_EXCUTE_STATUS_REVOKE:
                default:
                    throw new RestExceptionHandler(Message.TASK_EXE_STATUS_NO_ACTION);
            }

            // valid cmt
            // Remove cause enc
//			if (commentHandle) {
//				BussinessCommon.require("Ý kiến xử lý", comment);
//				BussinessCommon.validLengthData(comment, "Ý kiến xử lý", 2000);
//
//				TaskComment cmt = new TaskComment(comment, taskId);
//				cmt = taskCommentService.save(cmt);
//				taskAttService.addListAttachment(files, cmt.getId(), 2L);
//			}

            // update task status
            if (taskStatusTmp > 0) {
                task.setStatus(taskStatusTmp);
                taskRepository.save(task);
            }

            // update task exe status
            old.setStatus(status);
            taskExecuteRepository.save(old);

            // notification
            if (assignStatusNoti != null) {
                noticeService.add(task.getUserAssignId(), task.getId(), task.getTaskName(), DocumentTypeEnum.GIAO_VIEC,
                        assignStatusNoti, ModuleCodeEnum.TASK_ASSIGN, old.getUserId());
            }

            if (isRevokeFinish) { // delete noti of assigner
                noticeService.setActiveByUserIdAndDocIdAndDocType(task.getUserAssignId(), task.getId(), DocumentTypeEnum.GIAO_VIEC, false);
            }
        }
        return task;
    }

    /**
     * Restore task
     * Target : inactive all record except record of creator
     *
     * @param taskId
     */
    private void restoreTask(Long taskId) {
        List<TaskExecute> pList = taskExecuteService.getByTaskIdAndClientId(taskId);
        for (TaskExecute i : pList) {
            // Re-open record for create by
            if (com.vz.backend.core.config.Constant.START_STEP.equals(i.getStep())) {
                i.setStatus(Constant.TASK_EXCUTE_STATUS_INPROCESS);
            } else {
                i.setActive(false);
            }
            taskExecuteRepository.save(i);
        }
    }

    /**
     * Update status task exe
     *
     * @param input
     * @param status
     * @return
     */
    private List<TaskExecute> updateStatus(List<TaskExecute> input, int status) {
        List<TaskExecute> output = new ArrayList<>();
        if (BussinessCommon.isEmptyList(input))
            return output;
        input.forEach(i -> {
            i.setStatus(status);
            if (Constant.TASK_EXCUTE_STATUS_CLOSE == status) {
                i.setProgress(100);
            }
        });
        return taskExecuteRepository.saveAll(input);
    }

    /**
     * Danh sách công việc cá nhân
     *
     * @param assignedWork
     * @param userId
     * @param status
     * @param page
     * @return
     */
    public ListObjectDto<Task> findByUserExecute(Long userId, Long userAssignId, List<Integer> status, Integer page, Boolean checkParentId, List<Long> listId, Long parentId) {
        Page<Task> list = null;
        if (checkParentId) {
            list = taskRepository.findByUserExecute(ACTIVE, userId, userAssignId, status, listId,
                    BussinessCommon.castToPageable(page));
        } else {
            list = taskRepository.findTaskByUserExecuteAndParentNull(ACTIVE, userAssignId, userId, status, listId, parentId,
                    BussinessCommon.castToPageable(page));
        }
        for (Task task : list) {
            task.setTaskCombination(null);
            task.setTaskDocument(null);
            task.setUserExcute(null);
        }
        return BussinessCommon.paging(list);
    }

    public Page<TaskDto> findTaskByUserExecute(Long userId, List<Integer> status, Integer page) {
        return taskRepository.findTaskByUserExecute(true, userId, status, BussinessCommon.castToPageable(page));
    }

    /**
     * Danh sách công việc cá nhân
     *
     * @param compareDay
     * @param userId
     * @param status
     * @param page
     * @return
     */
    public ListObjectDto<TaskListDto> findByUserAssign(Integer dayLeft, List<Integer> status, Pageable page) {
        Long userId = BussinessCommon.getUserId();
        Long clientId = BussinessCommon.getClientId();
        Page<TaskListDto> rs = taskRepository.findByUserAssign(dayLeft, ACTIVE, userId, status, clientId, page);
        this.isAssignTab = true;
        setBtnAction(rs, true);
        return BussinessCommon.paging(rs);
    }

    public List<Task> listTaskStactistics() {
        List<Task> listData = taskRepository.findByListUserAssign(true, BussinessCommon.getUserId(), 1, BussinessCommon.getClientId());

        return listData;
    }

    public ListObjectDto<TaskExecuteDto> findByUserCombination(Integer dayLeft, Long userId, List<Integer> status, Boolean isCombination,
                                                               Pageable pageable) {
        Page<TaskExecuteDto> lstTasks = taskRepository.findByIdAndStatus(dayLeft, ACTIVE, status, isCombination, userId,
                BussinessCommon.getClientId(), pageable);
        return BussinessCommon.paging(lstTasks);
    }

    /**
     * Danh sách văn bản của công việc
     *
     * @param taskId
     * @return
     */
    public List<TaskDocument> getDocumentTask(Long taskId) {
        List<TaskDocument> taskDoc = taskDocumentRepository.findByTaskIdAndActiveTrueAndClientId(taskId, BussinessCommon.getClientId());
        for (TaskDocument element : taskDoc) {
            if (element.getTypeDocument()) {
                Documents doc = documentService.findById(element.getDocId()).get();
                element.setDocumentIn(doc);
            } else {
                DocumentOut doc = docOutService.findById(element.getDocId()).get();
                element.setDocumentOut(doc);
            }
        }
        return taskDoc;
    }

    public StatusTaskDto TaskStatusUser(Long userId) {
        StatusTaskDto statustask = new StatusTaskDto();
        statustask.setFinish(taskExecuteRepository.statusTask(userId, Constant.TASK_EXCUTE_STATUS_CLOSE).size());
        statustask.setProcessing(taskExecuteRepository.statusTask(userId, Constant.TASK_EXCUTE_STATUS_INPROCESS).size()
                + taskExecuteRepository.statusTask(userId, Constant.TASK_EXCUTE_STATUS_COMPLETE).size());
        statustask.setWaitingForReception(
                taskExecuteRepository.statusTask(userId, Constant.TASK_EXCUTE_STATUS_NEW).size());
        statustask.setOverDue(taskRepository.countOverDue(userId, BussinessCommon.getClientId()));
        statustask.setReturnTask(taskExecuteRepository.statusTask(userId, Constant.TASK_EXCUTE_STATUS_REJECT).size());
        return statustask;
    }

    /**
     * Get info execute by max step
     *
     * @param content
     */
    private void setData(List<TaskListDto> content) {
        List<Long> taskIds = content.stream().map(TaskListDto::getId).collect(Collectors.toList());
        List<TaskListDto> tmps = taskRepository.getUserByMaxStepAndTaskIds(taskIds, BussinessCommon.getClientId());
        Map<Long, TaskListDto> map = new HashMap<>();
        for (TaskListDto j : tmps) {
            map.put(j.getId(), j);
        }

        content.forEach(i -> {
            Long key = i.getId();
            if (map.containsKey(key)) {
                TaskListDto value = map.get(key);
                if (value == null)
                    return;
                i.setOrgName(value.getOrgName());
                i.setUserExcutePrimaryName(value.getUserExcutePrimaryName());
            }
        });
    }

    /**
     * Search by condition paging
     *
     * @param search
     * @param page
     * @return
     */
    public ListObjectDto<TaskListDto> findByMultiCondition(SearchTask search, Integer page) {
        List<TaskListDto> tmp = null;
        if (page == 0) {
            tmp = taskRepository.findByMultiCondition(search);
            setData(tmp);
            return BussinessCommon.convert(tmp);
        }
        Page<TaskListDto> rs = taskRepository.findByMultiCondition(search, BussinessCommon.castToPageable(page));
        if (!BussinessCommon.isEmptyPage(rs)) {
            tmp = rs.getContent();
            setData(tmp);
        }
        return BussinessCommon.paging(rs);
    }

    public List<TaskListDto> export(SearchTask search) {
        List<TaskListDto> data = taskRepository.findByMultiCondition(search);
        setData(data);
        return data;
    }

    /**
     * Search by condition not paging / config
     *
     * @param search
     * @return
     */
    public ObjectNode exportByConfig(SearchTask search) {
        List<TaskListDto> rs = taskRepository.findByMultiCondition(search);
        return rpFieldService.getData(DocumentTypeEnum.GIAO_VIEC, rs);
    }

    @Override
    public List<Task> findByClientId(Long clientId) {
        return taskRepository.findByClientId(clientId);
    }

    /**
     * Delete task
     *
     * @param taskId
     */
    public Boolean delete(Long taskId) {
        Task task = valid(taskId, Message.TASK_NOT_FOUND);
        Long userId = BussinessCommon.getUserId();
        if (!userId.equals(task.getCreateBy())) {
            throw new RestExceptionHandler(Message.ACTION_FAILED);
        }

        if (task.getStatus() == Constant.TASK_STATUS_CLOSE) {
            throw new RestExceptionHandler(Message.DELETE_DONE_TASK_NOT_ALLOWED);
        }

        weService.removeTaskId(taskId);
        List<Task> listTaskDeleteParentId = taskRepository.findByParentIdAndActive(BussinessCommon.getClientId(), taskId);
        if (listTaskDeleteParentId != null) {
            for (Task deletParent : listTaskDeleteParentId
            ) {
                deletParent.setParentId(null);
            }
            taskRepository.saveAll(listTaskDeleteParentId);
        }
        task.setActive(false);
        taskRepository.save(task);
        return true;
    }

    public TaskExecute progressReport(Long taskId, Integer progress, String comment) {
        BussinessCommon.validLengthData(comment, "Nội dung báo cáo", 200);
        TaskExecute taskExe = taskExecuteService.findByTaskAndUserId(taskId);
        if (taskExe == null) {
            throw new RestExceptionHandler(Message.TASK_NOT_FOUND);
        }
        if (progress == null || progress.intValue() <= 0 || progress.intValue() > 100) {
            throw new RestExceptionHandler(Message.UPDATE_PROCESS);
        }
        taskExe.setProgress(progress);
        taskExe.setComment(comment);
        return taskExecuteService.save(taskExe);
    }

    public ListObjectDto<Task> getListTaskUserLead(Integer dayLeft, List<Integer> status, Long orgId, Integer page) {

        User user = BussinessCommon.getUser();

        if (!user.getOrg().equals(orgId)) {
            throw new RestExceptionHandler(Message.TASK_NOT_FOUND);
        }

        List<Long> orgIds = orgService.orgAndSub(orgId);

        return BussinessCommon
                .paging(taskRepository.getListTaskLead(dayLeft, ACTIVE, status, orgIds, BussinessCommon.castToPageable(page)));
    }

    public List<TaskDto> findTaskByIds(List<Long> ids) {
        return taskRepository.findTaskByIds(ids);
    }

    public Task updateIsImportant(Long taskId) {
        Task ob = taskRepository.getOne(taskId);
        if (ob.getImportant() == null || ob.getImportant() == false) {
            ob.setImportant(true);
            taskRepository.save(ob);
        } else {
            ob.setImportant(false);
            taskRepository.save(ob);
        }
        return ob;
    }

    public List<TaskDto> getListTaskDtoByDocIdAndDocType(Long docId, boolean docType) {
        return taskRepository.getListTaskDtoByDocIdAndDocType(docId, docType);
    }

    public List<Task> getListTaskByDocIdAndDocType(Long docId, boolean docType) {
        return taskRepository.getListTaskByDocIdAndDocType(docId, docType);
    }

    public ReportDocByTypeDto reportDocByType() {
        // migrateByThread
        if (migrateByThread) {
            this.migrateByThread();
        }
        User user = BussinessCommon.getUser();
        List<ReportDocByTypeDto> rsList = taskRepository.reportDocByType(user.getId(), user.getClientId());
        rsList.addAll(weService.report());
        return new ReportDocByTypeDto(rsList);
    }

    public List<Task> getSubTasks(Long id, List<Long> idList) {
        if (id == null)
            return new ArrayList<>();
        List<Task> subTask = taskRepository.findByParentIdAndClientIdAndActiveTrue(id, BussinessCommon.getClientId());
        if (subTask.isEmpty())
            return new ArrayList<>();
        idList.add(id);

        for (Task i : subTask) {
            if (idList.contains(i.getId()))
                continue;
            i.setSubTasks(getSubTasks(i.getId(), idList));
            idList.add(i.getId());
        }
        return subTask;
    }

    public List<Task> getSubTasks(Long id) {
        Long clientId = BussinessCommon.getClientId();
        List<Task> subTask = taskRepository.findByParentIdAndClientIdAndActiveTrue(id, clientId);
        for (Task i : subTask) {
            if (i.getParentId() != null)
                i.setSubTasks(taskRepository.findByParentIdAndClientIdAndActiveTrue(i.getId(), clientId));
        }
        return subTask;
    }

    public List<Task> taskOrg(Long taskId) {
        Long parentId = BussinessCommon.getUser().getOrgModel().getParentId();
        Long orgId = parentId != null ? parentId : BussinessCommon.getUser().getOrg();
        List<Long> orgIds = orgService.orgAndSub(orgId);
        List<Task> orgTask = taskRepository.findByClientIdAndOrgIdInAndActiveTrue(BussinessCommon.getClientId(),
                orgIds);
        Set<Long> taskAndSub = taskAndSub(taskId, new HashSet<>());

        if (!taskAndSub.isEmpty()) {
            orgTask.removeIf(i -> taskAndSub.contains(i.getId()));
        }
        return orgTask;
    }

    public void validParentId(Long taskId, Long parentId) {
        if (taskId == null || parentId == null) return;
        Set<Long> taskAndSub = taskAndSub(taskId, new HashSet<>());
        if (taskAndSub.contains(parentId)) throw new RestExceptionHandler(Message.PARENT_TASK_INVALID);
    }

    public Set<Long> taskAndSub(Long taskId, Set<Long> rs) {
        if (taskId == null)
            return rs;
        rs.add(taskId);
        List<Task> tList = taskRepository.findByParentIdAndClientIdAndActiveTrue(taskId, BussinessCommon.getClientId());
        if (!tList.isEmpty()) {
            for (Task task : tList) {
                taskAndSub(task.getId(), rs);
            }
        }
        return rs;
    }

    public List<TaskRelated> getRelatedTask(Long taskId) {
        return taskRelatedRepository.findByTaskIdAndClientIdAndActiveTrue(taskId, BussinessCommon.getClientId());
    }

    public void addToTask(Long taskId, List<TaskRelated> nObjList) {
        List<TaskRelated> oObjList = getRelatedTask(taskId);
        List<TaskRelated> all = new ArrayList<>();
        all.addAll(nObjList);
        all.addAll(oObjList);

        all.forEach(i -> {
            // add
            if (nObjList.contains(i) && !oObjList.contains(i)) {
                i.setTaskId(taskId);
                //taskRelatedRepository.save(i);
            }

            // del
            if (!nObjList.contains(i) && oObjList.contains(i)) {
                i.setActive(false);
                //taskRelatedRepository.save(i);
            }
        });

        taskRelatedRepository.saveAll(all);
    }

    public List<KPIDataDto> findAllByToUser(List<Long> userIds, Date startDate, Date endDate) {
        return taskRepository.findAllByToUser(userIds, BussinessCommon.getClientId(), startDate, endDate);
    }

    public List<KPIDataDto> findAllByToUser(Long userId, Date startDate, Date endDate) {
        return taskRepository.findAllByToUser(userId, BussinessCommon.getClientId(), startDate, endDate);
    }

    public void saveSubTask(List<Task> nTasks, Long parentId) {
        List<Task> oTasks = taskRepository.findByParentIdAndClientIdAndActiveTrue(parentId,
                BussinessCommon.getClientId());
        HashMap<Long, Task> map = new HashMap<>();
        for (Task i : oTasks) {
            map.put(i.getId(), i);
        }

        if (nTasks == null)
            return;
        for (Task i : nTasks) {
            if (map.containsKey(i.getId())) {
                map.put(i.getId(), null);
                continue;
            }

            Optional<Task> tmp = findById(i.getId());
            if (tmp.isPresent()) {
                tmp.get().setParentId(parentId);
                taskRepository.save(tmp.get());
            }
        }

        map.forEach((k, v) -> {
            if (v != null) {
                v.setParentId(null);
                taskRepository.save(v);
            }
        });
    }

    public Page<TaskDto> getSubTasks(int page) {
        Pageable pageable = BussinessCommon.castToPageable(page);
        User u = BussinessCommon.getUser();
        Long parentId = u.getOrgModel().getParentId();
        Long orgId = parentId != null ? parentId : u.getOrg();
        List<Long> orgIds = orgService.orgAndSub(orgId);
        return taskRepository.getSubTasks(orgIds, u.getClientId(), pageable);
    }

    /**
     * Get task list
     *
     * @param isExcute
     * @param statusList
     * @param dayLeft
     * @param pageable
     * @return
     */
    public Page<TaskListDto> listByTypeAndStatus(Boolean isExcute, List<Integer> statusList, Integer dayLeft, boolean noCheckRead,
                                                 Pageable pageable) {
        Long userId = BussinessCommon.getUserId();
        Long clientId = BussinessCommon.getClientId();
        Page<TaskListDto> rs = taskRepository.listByTypeAndStatus(userId, isExcute, statusList, dayLeft, clientId,
                pageable);
        this.isAssignTab = false;
        setBtnAction(rs, noCheckRead);
        return rs;
    }

    /**
     * Set button action
     *
     * @param rs
     * @param noCheckRead : done tab, assign tab -> read is true
     */
    private void setBtnAction(Page<TaskListDto> rs, boolean noCheckRead) {
        if (BussinessCommon.isEmptyPage(rs))
            return;
        List<TaskListDto> content = rs.getContent();
        List<Long> nodeIds = content.stream().map(TaskListDto::getNodeId).distinct().collect(Collectors.toList());
        Map<Long, List<NodeDto>> nextNodeIds = bpmnService.getNextNodes(nodeIds, TYPE_DOCUMENT.ASSIGN);
        Map<Long, List<NodeDto>> reviewRequireds = bpmnService.getDataByNodeId(nodeIds);
        List<Long> taskIds = content.stream().map(TaskListDto::getId).collect(Collectors.toList());
        Map<Long, Boolean> objReadMap = objReadService.getObjReadMap(BussinessCommon.getUserId(), taskIds, DocumentTypeEnum.GIAO_VIEC);
        List<NodeDto> pNodes = new ArrayList<>();
        List<NodeDto> requireNodes = new ArrayList<>();
        for (TaskListDto i : content) {
            Long key = i.getNodeId();
            if (nextNodeIds.containsKey(key)) {
                pNodes = nextNodeIds.get(key);
            }
            if (reviewRequireds.containsKey(key)) {
                requireNodes = reviewRequireds.get(key);
            }
            this.setNodeInfo(i, pNodes, requireNodes);
            setBtnByTaskStatus(i);

            if (noCheckRead) {
                i.setRead(true);
            } else {
                i.setRead(objReadService.setRead(i.getId(), objReadMap));
            }
        }
    }

    /**
     * Set node info
     *
     * @param i
     * @param pNodes
     */
    private void setNodeInfo(TaskListDto i, List<NodeDto> pNodes, List<NodeDto> requireNodes) {
        i.setLastNode(bpmnService.lastNode(pNodes));
        i.setHasNextNode(bpmnService.hasNextNode(pNodes));
        i.setReviewRequired(bpmnService.reviewRequired(requireNodes));
    }

    /**
     * Set button action by task status
     *
     * @param info
     */
    private boolean isAssignTab = false;

    private void setBtnByTaskStatus(TaskListDto info) {
        TaskButtonActionDto btn = info.getButton();
        boolean isOwner = BussinessCommon.getUserId().equals(info.getCreateBy());
        btn.setAssigner(isOwner);
        switch (info.getTaskStatus()) {
            case Constant.TASK_STATUS_NEW:
                if (isOwner && this.isAssignTab) {
                    btn.setCanDelete(ButtonStatusEnum.ENABLE);
                    btn.setCanEdit(ButtonStatusEnum.ENABLE);
                    if (Boolean.TRUE.equals(info.getHasNextNode())) {
                        btn.setCanTransfer(ButtonStatusEnum.ENABLE);
                    }

                    break;
                }

                break;
            case Constant.TASK_STATUS_REJECT:
                if (isOwner && this.isAssignTab) {
                    btn.setCanDelete(ButtonStatusEnum.ENABLE);
                    btn.setCanEdit(ButtonStatusEnum.ENABLE);
                    btn.setCanRevoke(ButtonStatusEnum.ENABLE);
                    if (info.getNextNode() != null) {
                        btn.setCanAddTransfer(ButtonStatusEnum.ENABLE);
                    }
                    break;
                }

                this.setBtnByHandleStatus(info);
                break;
            case Constant.TASK_STATUS_INPROCESS:
                if (isOwner && this.isAssignTab) {
                    btn.setCanEdit(ButtonStatusEnum.ENABLE);
                    btn.setCanRevoke(ButtonStatusEnum.ENABLE);
                    btn.setCanDone(ButtonStatusEnum.ENABLE);
                    if (info.getNextNode() != null) {
                        btn.setCanAddTransfer(ButtonStatusEnum.ENABLE);
                    }
                    break;
                }

                this.setBtnByHandleStatus(info);
                break;
            case Constant.TASK_STATUS_COMPLETE:
                if (isOwner && this.isAssignTab) {
                    btn.setCanEdit(ButtonStatusEnum.ENABLE);
                    btn.setCanClose(ButtonStatusEnum.ENABLE);
                    btn.setCanRejectApprove(ButtonStatusEnum.ENABLE);
                    if (info.getNextNode() != null) {
                        btn.setCanAddTransfer(ButtonStatusEnum.ENABLE);
                    }
                    break;
                }

                this.setBtnByHandleStatus(info);
                break;
            case Constant.TASK_STATUS_CLOSE:
                break;
            case Constant.TASK_STATUS_REVOKE:
                if (isOwner && this.isAssignTab) {
                    btn.setCanRestore(ButtonStatusEnum.ENABLE);
                    break;
                }
                break;
            default:
                break;
        }
    }

    /**
     * Set button action by handle status
     *
     * @param info
     */
    private void setBtnByHandleStatus(TaskListDto info) {
        TaskButtonActionDto btn = info.getButton();
        if (info.getHandleStatus() == null)
            return;

        boolean passReviewRequired = !Boolean.TRUE.equals(info.getReviewRequired())
                || Boolean.TRUE.equals(info.getReviewRequiredResult());
        switch (info.getHandleStatus()) {
            case Constant.TASK_EXCUTE_STATUS_NEW:
                btn.setCanTodo(ButtonStatusEnum.ENABLE);
                btn.setCanReject(ButtonStatusEnum.ENABLE);
                break;
            case Constant.TASK_EXCUTE_STATUS_INPROCESS:
                if (Boolean.TRUE.equals(info.getHasNextNode()) && passReviewRequired) {
                    btn.setCanTransfer(ButtonStatusEnum.ENABLE);
                }

//			if (Boolean.TRUE.equals(info.getLastNode()) && passReviewRequired) {
                btn.setCanDone(ButtonStatusEnum.ENABLE);
//			}

                if (info.getNextNode() != null) {
                    btn.setCanAddTransfer(ButtonStatusEnum.ENABLE);
                }
                break;
            case Constant.TASK_EXCUTE_STATUS_REJECT:
                btn.setCanTodo(ButtonStatusEnum.ENABLE);
                break;
            case Constant.TASK_EXCUTE_STATUS_COMPLETE:
                if (Boolean.TRUE.equals(info.getClose())) {
                    btn.setCanRevokeFinish(ButtonStatusEnum.ENABLE);
                }
                if (info.getNextNode() != null) {
                    btn.setCanAddTransfer(ButtonStatusEnum.ENABLE);
                }
                break;
            default:
                break;
        }
    }

    /**
     * Get button action by taskId
     *
     * @param taskId
     * @param assigner
     * @return
     */
    public TaskButtonActionDto getActionByTaskId(Long taskId, Boolean assigner) {
        this.isAssignTab = Boolean.TRUE.equals(assigner) ? assigner : false;
        Long userId = BussinessCommon.getUserId();
        Task task = valid(taskId, Message.TASK_NOT_FOUND);
        TaskExecute old = taskExecuteService.findByTaskIdAndUserId(taskId, userId);
        TaskListDto dto;
        if (old == null) {
            dto = new TaskListDto(task, null, null, null, false, false, null);
            setBtnByTaskStatus(dto);
            return dto.getButton();
        }

        dto = new TaskListDto(task, old.getStatus(), old.getNode(), old.getReviewRequired(), false, old.getClose(), old.getNextNode());
        List<NodeDto> nodes = bpmnService.nodeList(old.getNode(), TYPE_DOCUMENT.ASSIGN);
        this.setNodeInfo(dto, nodes, new ArrayList<>());
        dto.setReviewRequired(bpmnService.getReviewRequiredByNodeId(old.getNode()));
        setBtnByTaskStatus(dto);
        return dto.getButton();
    }

    /**
     * Migrate data when apply thread to task
     *
     * @return
     */
    public Boolean migrateByThread() {
        // Add miss task_exe record
        // Insert history
        Long clientId = BussinessCommon.getClientId();
        List<Task> tasks = taskRepository.getTaskNotRecordFirstStepYet(clientId);
        List<TaskHistory> taskHistorys = new ArrayList<>();
        if (tasks.isEmpty())
            return true;
        List<TaskExecute> rs = new ArrayList<>();
        for (Task t : tasks) {
            rs.add(new TaskExecute(t));
            if (t.getStatus() == 0) {
                t.setStatus(1);
            }

            // Task history mapping create of task
            TaskHistory th = taskHistoryRepository.save(new TaskHistory(t));
            th.setCreateBy(t.getCreateBy());
            th.setCreateDate(t.getCreateDate());
            taskHistorys.add(th);
        }

        try {
            taskExecuteRepository.saveAll(rs);
            taskRepository.saveAll(tasks);
            taskHistoryRepository.saveAll(taskHistorys);
        } catch (Exception e) {
            e.printStackTrace();
        }

        // Update task_exe record
        rs.clear();
        List<TaskExecute> taskExes = taskRepository.getTaskExeIsNull(clientId);
        if (taskExes.isEmpty())
            return true;
        for (TaskExecute tx : taskExes) {
            Task tmp = tx.getTask();
            if (tmp == null)
                continue;
            tx.setStep(2);
            tx.setFrUserId(tmp.getCreateBy());
            rs.add(tx);
        }

        try {
            taskExecuteRepository.saveAll(rs);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return true;
    }

    public List<TaskExecuteByNodeDto> getUserByNodeId(Long taskId, Long nodeId) {
        return taskExecuteRepository.getUserByNodeId(taskId, nodeId, BussinessCommon.getClientId());
    }
}
