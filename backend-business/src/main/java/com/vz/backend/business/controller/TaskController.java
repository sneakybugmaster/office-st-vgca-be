package com.vz.backend.business.controller;

import java.util.*;

import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.service.OrganizationService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.jpa.domain.JpaSort;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.domain.DocumentOutTask;
import com.vz.backend.business.domain.Task;
import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.business.domain.TaskDocument;
import com.vz.backend.business.domain.TaskExecute;
import com.vz.backend.business.domain.TaskHistory;
import com.vz.backend.business.dto.DocumentBasicDto;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.business.dto.SearchTask;
import com.vz.backend.business.dto.TaskDto;
import com.vz.backend.business.dto.TaskExecuteByNodeDto;
import com.vz.backend.business.dto.task.TaskButtonActionDto;
import com.vz.backend.business.dto.task.TaskExecuteTrackingDto;
import com.vz.backend.business.dto.task.TaskListDto;
import com.vz.backend.business.dto.task.TaskTransferDto;
import com.vz.backend.business.repository.ITaskRepository;
import com.vz.backend.business.service.ObjectReadService;
import com.vz.backend.business.service.TaskAttachmentService;
import com.vz.backend.business.service.TaskDocumentService;
import com.vz.backend.business.service.TaskExecuteService;
import com.vz.backend.business.service.TaskHistoryService;
import com.vz.backend.business.service.TaskService;
import com.vz.backend.business.service.WordEditorService;
import com.vz.backend.business.util.Constant;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.IService;
import com.vz.backend.core.service.UserService;

/**
 * @author DucND
 * @date May 29, 2020
 */
@RestController
@RequestMapping("/task")
public class TaskController {
	
	@Autowired
	ITaskRepository taskRepository;

	@Autowired
	TaskService taskService;
	
	@Autowired
	TaskExecuteService taskExecuteService;
	
	@Autowired
	TaskDocumentService taskDocumentService;

	@Autowired
	TaskAttachmentService attachservice;
	
	@Autowired
	WordEditorService weService;
	
	@Autowired
	UserService userService;
	
	@Autowired
	TaskHistoryService taskHistoryService;
	
	@Autowired
	ObjectReadService objReadService;

	@Autowired
	OrganizationService organizationService;

	public IService<Task> getService() {
		return taskService;
	}
	
	enum SortBy {
		UPDATEDATE("updateDate"), // Ngày cập nhật
		CREATEDATE("createDate"), // Ngày tạo
		NAME("taskName"), // tên công việc
		START_DATE("startDate"), // Ngày bắt đầu
		END_DATE("endDate"), // ngày kết thúc
		CODE("codeTask"), // mã cv
//		TASK_STATUS("status"), // trạng thái công việc
		STATUS_TASK("(CASE WHEN status = 0 THEN 'Mới giao' "
				+ "WHEN status = 1 THEN 'Đang thực hiện' "
				+ "WHEN status = 2 THEN 'Từ chối' "
				+ "WHEN status = 3 THEN 'Hoàn thành (Chờ đánh giá)' "
				+ "ELSE 'Close' END)"),
		STATUS_TASK_EXE("(CASE WHEN t.status = 0 THEN 'Mới giao' "
				+ "WHEN t.status = 1 THEN 'Đang thực hiện' "
				+ "WHEN t.status = 2 THEN 'Từ chối' "
				+ "WHEN t.status = 3 THEN 'Hoàn thành (Chờ đánh giá)' "
				+ "ELSE 'Close' END)"),
		PERSON_HANDLE("userAssign.fullName"), // người xử lý ( tab phối hợp)
		IMPORTANT("(CASE WHEN t.important is null THEN 3 WHEN t.important is FALSE THEN 2 ELSE 1 END)"), // Quan trọng
		IMPORTANT_EXE("(CASE WHEN tx.important is null THEN 3 WHEN tx.important is FALSE THEN 2 ELSE 1 END)"), // Quan trọng
		;
		
		private String field;

		private SortBy(String field) {
			this.field = field;
		}
	}
	
	private final String main = "main";
	private final String support = "support";
	private final String done = "done";
	private final String notyet = "notyet";

	@GetMapping("/findById/{id}")
	public ResponseEntity<Task> finById(@PathVariable Long id) {
		Task task = taskService.valid(id, Message.TASK_NOT_FOUND);
		taskService.checkPermission(task);
		task.setTaskDocument(taskService.getDocumentTask(id));
		List<TaskAttachment> attList = attachservice.findByObjId(task.getId(),
				com.vz.backend.core.config.Constant.TYPE_TASK, BussinessCommon.getClientId(), true);
		task.setAttachments(attList);
		List<DocumentBasicDto> listDocOutReply = new ArrayList<>();
		for (DocumentOutTask d : task.getListDocumentOutTask()) {
			listDocOutReply.add(new DocumentBasicDto(d.getDocOut()));
		}
		
		task.setListDocOutReply(listDocOutReply);
		task.setSubTasks(taskService.getSubTasks(task.getId()));
		task.setWeList(weService.getByTaskId(task.getId()));
		task.setWeIds();
		task.setTaskRelateds(taskService.getRelatedTask(task.getId()));
		task.setTaskHistorys(taskHistoryService.findByTaskId(task.getId()));
		
		// Set read
		TaskExecute taskExe = taskExecuteService.findByTaskIdAndUserId(id, BussinessCommon.getUserId());
		if (taskExe != null) {
			taskExe.setRead(true);
			taskExecuteService.save(taskExe);
			task.setNodeId(taskExe.getNode());
			task.setNextNode(taskExe.getNextNode());
		}
		
		objReadService.setRead(BussinessCommon.getUserId(), id, DocumentTypeEnum.GIAO_VIEC, true);
		return new ResponseEntity<>(task, HttpStatus.OK);
	}

	@PostMapping(value = "/addTaskAssign")
	public ResponseEntity<Task> addTaskAssign(@RequestBody Task task) {
		List<TaskDocument> taskDocument = task.getTaskDocument();
		task.setPriority(null);
		task.setField(null);
		task.setUserAssign(null);
		task.setUserExcute(null);
		task.valids();
		taskService.validParentId(task.getId(), task.getParentId());

		try {
			Long userId = BussinessCommon.getUserId();
			Task taskInfo = taskService.save(task);
			if (taskInfo.getCodeTask().split("_").length == 3) {
				Organization organization = BussinessCommon.getUser().getOrgModel();
				organization.setNumberTaskInYear(Integer.parseInt(taskInfo.getCodeTask().split("_")[2]));
				organization.setCurrentYear(Integer.parseInt(taskInfo.getCodeTask().split("_")[1]));
				organizationService.save(organization);
			}
			taskService.addToTask(taskInfo.getId(), task.getTaskRelateds());
			weService.addToTask(taskInfo.getId(), task.getWeListId());
			taskService.saveSubTask(task.getSubTasks(), task.getId());
			taskExecuteService.save(taskInfo.getId(), userId);
			taskHistoryService.save(taskInfo);
			if (taskDocument != null && !taskDocument.isEmpty()) {
				for (TaskDocument element : taskDocument) {
					element.setTaskId(taskInfo.getId());
					element.setDocumentIn(null);
					element.setDocumentOut(null);
				}
			}
			taskDocumentService.saveTaskDocument(task.getId(), taskDocument);
			taskInfo.setTaskDocument(taskDocument);
			taskInfo = taskService.findById(taskInfo.getId()).get();
			taskInfo.setUserAssign(userService.valid(taskInfo.getUserAssignId(), Message.ERROR_SYS));
			List<TaskAttachment> attList = attachservice.findByObjId(taskInfo.getId(),
					com.vz.backend.core.config.Constant.TYPE_TASK, BussinessCommon.getClientId(), true);
			taskInfo.setAttachments(attList);
			return new ResponseEntity<>(taskInfo, HttpStatus.OK);
		} catch (Exception e) {
			e.printStackTrace();
			throw new RestExceptionHandler(Message.ACTION_FAILED);
		}
	}

	@PostMapping("/addTaskDoc")
	public ResponseEntity<?> addTaskDoc(@RequestBody Task task) {
		List<TaskExecute> taskExecute = task.getTaskExecute();
		List<TaskDocument> taskDocument = task.getTaskDocument();
		task.setField(null);
		task.setPriority(null);
		task.setUserAssign(null);
		task.setUserExcute(null);
		Task taskInfo = taskService.save(task);
		for (TaskExecute element : taskExecute) {
			element.setTaskId(taskInfo.getId());
			element.setUser(null);
			element.setStatus(Constant.TASK_EXCUTE_STATUS_NEW);
		}
		
		for (TaskDocument element : taskDocument) {
			element.setTaskId(taskInfo.getId());
			element.setDocumentIn(null);
			element.setDocumentOut(null);
		}
		
		taskExecuteService.saveAll(taskExecute);
		taskDocumentService.saveAll(taskDocument);
		
		return new ResponseEntity<>(taskInfo, HttpStatus.OK);
	}

	@PostMapping("/addUserApprove")
	public ResponseEntity<?> addUserApprove(@RequestBody List<TaskExecute> taskExecutes) {
		for (TaskExecute taskExecute : taskExecutes) {
			taskExecuteService.save(taskExecute);
		}
		return new ResponseEntity<>(HttpStatus.OK);
	}

	/**
	 * Danh sách công việc đã giao
	 *
	 * @param userId
	 * @param status
	 * @param page
	 * @return
	 */
	@PostMapping(value = "/findByUserAssign")
	public ResponseEntity<?> findByUserAssign(@RequestParam(value = "status") Boolean statusTask,
			@RequestParam(value = "dayLeft", required = false) Integer dayLeft,
			@RequestParam(defaultValue = com.vz.backend.core.config.Constant.DEFAULT_SORT_BY) SortBy sortBy,
			@RequestParam(defaultValue = com.vz.backend.core.config.Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(value = "size", defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_NUMBER) int page) {
		List<Integer> status = new ArrayList<>();
		if (Boolean.TRUE.equals(statusTask)) {
			status.add(Constant.TASK_STATUS_CLOSE);
			status.add(Constant.TASK_STATUS_REVOKE);
		} else {
			status.add(Constant.TASK_STATUS_NEW);
			status.add(Constant.TASK_STATUS_REJECT);
			status.add(Constant.TASK_STATUS_INPROCESS);
			status.add(Constant.TASK_STATUS_COMPLETE);
		}

		Sort sort = JpaSort.unsafe(direction, sortBy.field);
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(taskService.findByUserAssign(dayLeft, status, pageable), HttpStatus.OK);
	}

	/**
	 * Danh sách công việc cá nhân
	 *
	 * @param userId
	 * @param status
	 * @param page
	 * @return
	 */
	@PostMapping(value = "/findByUserExecute")
	public ResponseEntity<?> findByUserExecute(@RequestParam(value = "status", required = false) Boolean statusTask,
			@RequestParam(value = "page") String page,
			@RequestParam(value = "assignedWork", defaultValue = "FALSE") Boolean assignedWork, @RequestParam(value = "checkParentId") Boolean checkParentId
	,@RequestParam(value = "listId") List<Long> listId,@RequestParam(value = "parentId") Long parentId) {
		List<Integer> status = new ArrayList<>();

		if (statusTask == null) {
			status.add(Constant.TASK_STATUS_CLOSE);
			status.add(Constant.TASK_STATUS_NEW);
			status.add(Constant.TASK_STATUS_REJECT);
			status.add(Constant.TASK_STATUS_INPROCESS);
			status.add(Constant.TASK_STATUS_COMPLETE);
		} else if (Boolean.TRUE.equals(statusTask)) {
			status.add(Constant.TASK_STATUS_CLOSE);
			status.add(Constant.TASK_STATUS_REVOKE);
		} else {
			status.add(Constant.TASK_STATUS_NEW);
			status.add(Constant.TASK_STATUS_REJECT);
			status.add(Constant.TASK_STATUS_INPROCESS);
			status.add(Constant.TASK_STATUS_COMPLETE);
		}

		Long userId = null;
		Long userAssignId = null;
		if(Boolean.TRUE.equals(assignedWork))  {
			userId = BussinessCommon.getUserId();
		} else {
			userAssignId = BussinessCommon.getUserId();
		}
		
		return new ResponseEntity<>(
				taskService.findByUserExecute(userId, userAssignId, status, BussinessCommon.setPageNumber(page),checkParentId,listId,parentId),
				HttpStatus.OK);
	}

	@GetMapping(value = "/findTaskByUserExecute")
	public ResponseEntity<Page<TaskDto>> findTaskByUserExecute(
			@RequestParam(value = "status", required = false) Boolean statusTask,
			@RequestParam(value = "page") String page) {
		List<Integer> status = new ArrayList<>();

		if (statusTask == null) {
			status.add(Constant.TASK_STATUS_CLOSE);
			status.add(Constant.TASK_STATUS_NEW);
			status.add(Constant.TASK_STATUS_REJECT);
			status.add(Constant.TASK_STATUS_INPROCESS);
			status.add(Constant.TASK_STATUS_COMPLETE);
		} else if (Boolean.TRUE.equals(statusTask)) {
			status.add(Constant.TASK_STATUS_CLOSE);
			status.add(Constant.TASK_STATUS_REVOKE);
		} else {
			status.add(Constant.TASK_STATUS_NEW);
			status.add(Constant.TASK_STATUS_REJECT);
			status.add(Constant.TASK_STATUS_INPROCESS);
			status.add(Constant.TASK_STATUS_COMPLETE);
		}

		return new ResponseEntity<>(taskService.findTaskByUserExecute(BussinessCommon.getUserId(), status,
				BussinessCommon.setPageNumber(page)), HttpStatus.OK);
	}

	/**
	 * Hiển thị tất cả người tham gia công việc
	 *
	 * @param userId
	 * @param status
	 * @param pageable
	 * @return
	 */
	@PostMapping(value = "/listTaskUserExecute")
	public ResponseEntity<?> listTaskUserExecute(@RequestParam(value = "taskId") Long taskId) {
		return new ResponseEntity<>(taskExecuteService.findByTaskId(taskId), HttpStatus.OK);
	}

	/**
	 * Danh sách công việc hỗ trợ hiển thị công việc đã hoàn thành người dùng tham
	 * gia phụ việc
	 *
	 * @param userId
	 * @return
	 */
	@PostMapping("/findByUserCombination1")
	public ResponseEntity<?> findByUserCombination(
			@RequestParam(value = "userId") Long userId,
			@RequestParam(value = "dayLeft", required = false) Integer dayLeft, 
			@RequestParam(value = "iscombination") Boolean iscombination,
			@RequestParam(value = "status") Boolean statusTask, 
			@RequestParam(defaultValue = com.vz.backend.core.config.Constant.DEFAULT_SORT_BY) SortBy sortBy,
			@RequestParam(defaultValue = com.vz.backend.core.config.Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(value = "size", defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_NUMBER) int page) {
		List<Integer> status = new ArrayList<>();
		if (Boolean.TRUE.equals(statusTask)) {
			status.add(Constant.TASK_STATUS_CLOSE);
			status.add(Constant.TASK_STATUS_REVOKE);
		} else {
			status.add(Constant.TASK_STATUS_NEW);
			status.add(Constant.TASK_STATUS_REJECT);
			status.add(Constant.TASK_STATUS_INPROCESS);
			status.add(Constant.TASK_STATUS_COMPLETE);
		}
		
		Sort sort = JpaSort.unsafe(direction, sortBy.field);
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(taskService.findByUserCombination(dayLeft, userId, status, iscombination, pageable),
				HttpStatus.OK);
	}

	@PostMapping("/getDocumentTask")
	public ResponseEntity<?> getDocumentTask(@RequestParam(value = "idTask") Long idTask) {
		return new ResponseEntity<>(taskService.getDocumentTask(idTask), HttpStatus.OK);
	}

	@GetMapping("/statusTask/{userId}")
	public ResponseEntity<?> statusTask(@PathVariable(value = "userId") Long userId) {
		return new ResponseEntity<>(taskService.TaskStatusUser(userId), HttpStatus.OK);
	}

	@PostMapping("/list/all/{page}")
	public ResponseEntity<?> findByTaskName(@PathVariable Integer page, @RequestBody SearchTask search) {
		Long userAssignId = null;
		Long userExcutePrimaryId = null;
		Long userId = BussinessCommon.getUserId();
		if (Boolean.TRUE.equals(search.getTaskType())) {// Tìm theo giao việc
			userAssignId = userId;
		} else if (Boolean.FALSE.equals(search.getTaskType())) {
			userExcutePrimaryId = userId;
		}

		search.convert(userAssignId, userExcutePrimaryId);
		return new ResponseEntity<>(taskService.findByMultiCondition(search, page), HttpStatus.OK);
	}
	
	@PostMapping("/export")
	public ResponseEntity<?> export(@RequestBody SearchTask search) {
		Long userAssignId = null;
		Long userExcutePrimaryId = null;
		Long userId = BussinessCommon.getUserId();
		if (Boolean.TRUE.equals(search.getTaskType())) {// Tìm theo giao việc
			userAssignId = userId;
		} else if (Boolean.FALSE.equals(search.getTaskType())) {
			userExcutePrimaryId = userId;
		}

		search.convert(userAssignId, userExcutePrimaryId);
		if (Boolean.TRUE.equals(search.getConfigFields())) {
			return new ResponseEntity<>(taskService.exportByConfig(search), HttpStatus.OK);
		}
		return new ResponseEntity<>(taskService.export(search), HttpStatus.OK);
	}

	@PostMapping(value = "/update/{taskId}")
	public ResponseEntity<Task> updatetask(@RequestBody Task task, @PathVariable(value = "taskId") Long taskId) {
		Task old = taskService.valid(taskId, Message.TASK_NOT_FOUND);
		task.valids();
		taskService.validParentId(taskId, task.getParentId());
		old.set(task);
		old.setParentId(task.getParentId());
		taskService.saveSubTask(task.getSubTasks(), taskId);
		Task taskInfo = taskService.save(old);

		List<TaskDocument> taskDocumentOld = taskService.getDocumentTask(taskId);
		if(taskDocumentOld != null){
			taskDocumentService.deleteAll(taskDocumentOld);
		}
		List<TaskDocument> taskDocument = task.getTaskDocument();
		if (taskDocument != null && !taskDocument.isEmpty()) {
			for (TaskDocument element : taskDocument) {
				element.setTaskId(taskInfo.getId());
				element.setDocumentIn(null);
				element.setDocumentOut(null);
			}
		}
		taskDocument = taskDocumentService.saveAll(taskDocument);
		taskInfo.setTaskDocument(taskDocument);

		taskService.addToTask(taskInfo.getId(), task.getTaskRelateds());

		weService.addToTask(taskId, task.getWeListId());
		taskHistoryService.save(taskInfo);
		return new ResponseEntity<>(taskInfo, HttpStatus.OK);
	}

	@GetMapping("/deleteTaskExecute/{taskExId}")
	public ResponseEntity<?> deleteTaskExecute(@PathVariable(value = "taskExId") Long id) {
		taskExecuteService.deleteById(id);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@GetMapping("/findByClientId/{clientId}")
	public ResponseEntity<?> findByClientId(@PathVariable Long clientId) {
		return new ResponseEntity<>(taskService.findByClientId(clientId), HttpStatus.OK);
	}

	@GetMapping("/delete/{taskId}")
	public ResponseEntity<Boolean> deleteActive(@PathVariable Long taskId) {
		return new ResponseEntity<>(taskService.delete(taskId), HttpStatus.OK);
	}

	@PostMapping(value = "/progress_report/{taskId}")
	public ResponseEntity<TaskExecute> progressReport(@PathVariable Long taskId,
			@RequestParam(required = false, value = "progress") Integer progress,
			@RequestParam(required = false, value = "comment") String comment) {
		return new ResponseEntity<>(taskService.progressReport(taskId, progress, comment), HttpStatus.OK);
	}

	@PostMapping("/getListTaskUserLead")
	public ResponseEntity<?> getListTaskUserLead(
			@RequestParam(value = "dayLeft", required = false) Integer dayLeft,
			@RequestParam(value = "status", required = false) Boolean statusTask,
			@RequestParam(value = "orgId") Long orgId, @RequestParam(value = "page") int page) {
		List<Integer> status = new ArrayList<>();
		if (Boolean.TRUE.equals(statusTask)) {
			status.add(Constant.TASK_STATUS_CLOSE);
		} else {
			status.add(Constant.TASK_STATUS_NEW);
			status.add(Constant.TASK_STATUS_REJECT);
			status.add(Constant.TASK_STATUS_INPROCESS);
			status.add(Constant.TASK_STATUS_COMPLETE);
		}
		return new ResponseEntity<>(taskService.getListTaskUserLead(dayLeft, status, orgId, page), HttpStatus.OK);
	}

	@PostMapping("/updateImportant")
	public ResponseEntity<?> updateImportant(@RequestParam(value = "taskId") Long id) {
		return new ResponseEntity<>(taskService.updateIsImportant(id), HttpStatus.OK);
	}

	@PostMapping("/updateImportantTaskExecute")
	public ResponseEntity<?> updateImportantTaskExecute(@RequestParam(value = "taskId") Long taskId) {
		return new ResponseEntity<>(taskExecuteService.updateIsImportant(taskId), HttpStatus.OK);
	}
	
	@GetMapping("/report_doc_by_type")
	public ResponseEntity<ReportDocByTypeDto> reportDocByType() {
		return new ResponseEntity<>(taskService.reportDocByType(), HttpStatus.OK);
	}
	
	@GetMapping("/task-org")
	public ResponseEntity<List<Task>> taskOrg(@RequestParam(required = false) Long taskId) {
		return new ResponseEntity<>(taskService.taskOrg(taskId), HttpStatus.OK);
	}
	
	@GetMapping("/sub-task/{taskId}")
	public ResponseEntity<List<Task>> subTask(@PathVariable Long taskId) {
		return new ResponseEntity<>(taskService.getSubTasks(taskId), HttpStatus.OK);
	}
	
	@GetMapping("/task-sub/{id}")
	public ResponseEntity<Set<Long>> taskAndSub(@PathVariable Long id) {
		return new ResponseEntity<>(taskService.taskAndSub(id, new HashSet<>()), HttpStatus.OK);
	}
	
	@GetMapping("/sub/list/{page}")
	public ResponseEntity<Page<TaskDto>> getSubTasks(@PathVariable int page) {
		return new ResponseEntity<>(taskService.getSubTasks(page), HttpStatus.OK);
	}
	
	@PostMapping("/transfer/{taskId}")
	public ResponseEntity<?> transfer(@PathVariable Long taskId, @RequestBody TaskTransferDto dto) {
		return new ResponseEntity<>(taskExecuteService.transfer(taskId, dto), HttpStatus.OK);
	}
	
	@PostMapping("/add-transfer/{taskId}")
	public ResponseEntity<?> addTransfer(@PathVariable Long taskId, @RequestBody TaskTransferDto dto) {
		return new ResponseEntity<>(taskExecuteService.addTransfer(taskId, dto), HttpStatus.OK);
	}
	
	@GetMapping(value = "/tracking/list/{taskId}")
	public ResponseEntity<List<TaskExecuteTrackingDto>> tracking(@PathVariable Long taskId) {
		return new ResponseEntity<>(taskExecuteService.buildTree(taskId), HttpStatus.OK);
	}

	@GetMapping(value = "/tracking/listAll/{taskId}")
	public ResponseEntity<List<TaskExecute>> trackingAll(@PathVariable Long taskId) {
		return new ResponseEntity<>(taskExecuteService.getAll(taskId), HttpStatus.OK);
	}
	
	@GetMapping("/history/{taskHistoryId}")
	public ResponseEntity<TaskHistory> getTaskHistoryId(@PathVariable Long taskHistoryId) {
		return new ResponseEntity<>(taskHistoryService.valid(taskHistoryId, Message.TASK_NOT_FOUND), HttpStatus.OK);
	}
	
	@PostMapping(value = "/list/{type}/{status}")
	public ResponseEntity<Page<TaskListDto>> listByTypeAndStatus(@PathVariable String type, @PathVariable String status,
			@RequestParam(value = "dayLeft", required = false) Integer dayLeft,
			@RequestParam(defaultValue = com.vz.backend.core.config.Constant.DEFAULT_SORT_BY) SortBy sortBy,
			@RequestParam(defaultValue = com.vz.backend.core.config.Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(value = "size", defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_NUMBER) int page) {
		this.validType(type);
		this.validStatus(status);
		List<Integer> statusList = this.getStatus(status);
		Boolean isExcute = main.equals(type);
		Sort sort = JpaSort.unsafe(direction, sortBy.field);
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		boolean noCheckRead = done.equals(status);
		return new ResponseEntity<>(
				taskService.listByTypeAndStatus(isExcute, statusList, dayLeft, noCheckRead, pageable), HttpStatus.OK);
	}
	
	@GetMapping("/detail/action/{taskId}")
	public ResponseEntity<TaskButtonActionDto> getActionByTaskId(@PathVariable Long taskId,
			@RequestParam(required = false, defaultValue = "FALSE") Boolean assigner) {
		return new ResponseEntity<>(taskService.getActionByTaskId(taskId, assigner), HttpStatus.OK);
	}
	
	@PostMapping(value = "/update/status/{taskId}")
	public ResponseEntity<?> updateStatusByCondition(@PathVariable Long taskId, @RequestParam Integer status,
			@RequestParam(defaultValue = "FALSE", name = "isExcute") Boolean assigner, @RequestParam(required = false) String comment,
			@RequestParam(value = "files", required = false) MultipartFile[] files) {
		return new ResponseEntity<>(taskService.updateStatusByCondition(taskId, status, assigner, comment, files), HttpStatus.OK);
	}
	
	/**
	 * Valid type
	 * @param type
	 */
	private void validType(String type) {
		if (type == null)
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		switch (type) {
		case main:
		case support:
			break;
		default:
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}
	}
	
	/**
	 * Valid status
	 * @param status
	 */
	private void validStatus(String status) {
		if (status == null)
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		switch (status) {
		case done:
		case notyet:
			break;
		default:
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}
	}
	
	/**
	 * To int by status
	 * @param status
	 * @return
	 */
	private List<Integer> getStatus(String status) {
		List<Integer> rs = new ArrayList<>();
		switch (status) {
		case done:
			rs.add(Constant.TASK_STATUS_CLOSE);
			rs.add(Constant.TASK_STATUS_REVOKE);
			break;
		case notyet:
			rs.add(Constant.TASK_STATUS_NEW);
			rs.add(Constant.TASK_STATUS_REJECT);
			rs.add(Constant.TASK_STATUS_INPROCESS);
			rs.add(Constant.TASK_STATUS_COMPLETE);
			break;
		default:
			break;
		}
		return rs;
	}
	
	@GetMapping("/migrate/thread")
	public ResponseEntity<Boolean> migrateByThread() {
		return new ResponseEntity<>(taskService.migrateByThread(), HttpStatus.OK);
	}
	
	@GetMapping("/users/{nodeId}/{taskId}")
	public ResponseEntity<List<TaskExecuteByNodeDto>> getNodeUsers(@PathVariable Long nodeId, @PathVariable Long taskId) {
		List<TaskExecuteByNodeDto> users = taskService.getUserByNodeId(taskId, nodeId);
		return new ResponseEntity<>(users, HttpStatus.OK);
	}
}
