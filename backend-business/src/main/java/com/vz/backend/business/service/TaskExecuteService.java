package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.persistence.EntityManagerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.Task;
import com.vz.backend.business.domain.TaskExecute;
import com.vz.backend.business.domain.TaskHistory;
import com.vz.backend.business.dto.task.TaskExecuteTrackingDto;
import com.vz.backend.business.dto.task.TaskTransferDto;
import com.vz.backend.business.repository.ITaskExecuteRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.LabelValueDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.UserService;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Service
public class TaskExecuteService extends BaseService<TaskExecute> {

	@Autowired
	ITaskExecuteRepository taskRepository;

	@Autowired
	EntityManagerFactory entityManagerFactory;

	@Override
	public IRepository<TaskExecute> getRepository() {
		return taskRepository;
	}

	@Autowired
	private TaskService taskService;

	@Autowired
	private TaskHistoryService taskHistoryService;

	@Autowired
	private NotificationService noticeService;
	
	@Autowired
	private UserService userService;
	
	@Autowired
	ObjectReadService objReadService;

	/**
	 * Hiển thị tất cả người dùng trong công việc
	 *
	 * @param userId
	 * @param status
	 * @param pageable
	 * @return
	 */
	public List<TaskExecute> findByTaskId(Long taskId) {
		return taskRepository.findByTaskId(taskId, BussinessCommon.getClientId());
	}

	public List<Long> findUserIdByTaskId(Long taskId) {
		List<TaskExecute> tList = findByTaskId(taskId);
		return tList.stream().map(i -> i.getUserId()).collect(Collectors.toList());
	}

	/**
	 * find task which current user is doing
	 *
	 * @param taskId
	 * @return
	 */
	public TaskExecute findByTaskAndUserId(Long taskId) {
		User user = BussinessCommon.getUser();
		return taskRepository.findByTaskIdAndUserIdAndClientIdAndActive(taskId, user.getId(), user.getClientId(), true);
	}

	public TaskExecute updateIsImportant(Long taskId) {
		TaskExecute ob = findByTaskIdAndUserId(taskId, BussinessCommon.getUserId());
		if (ob.getImportant() == null || ob.getImportant() == false) {
			ob.setImportant(true);
			taskRepository.save(ob);
		} else {
			ob.setImportant(false);
			taskRepository.save(ob);
		}
		return ob;
	}

	public boolean userInTask(Long userId, Long taskId) {
		return taskRepository.userInTask(taskId, userId, BussinessCommon.getClientId());
	}
	
	private boolean checkStatusHandleCanTransfer(Integer status) {
		return (status < com.vz.backend.business.util.Constant.TASK_EXCUTE_STATUS_COMPLETE);
	}
	
	private boolean checkStatusHandleCanAddTransfer(Integer status) {
		return (status < com.vz.backend.business.util.Constant.TASK_EXCUTE_STATUS_CLOSE);
	}

	/**
	 * Transfer task execute
	 * 
	 * @param taskId
	 * @param dto
	 * @return
	 */
	public List<TaskExecute> transfer(Long taskId, TaskTransferDto dto) {
		// Target : save list task execute with rule 1 step + 1 userId is unique
		// Step 1: Validate data
		// Step 2: Save history task
		// Step 3: Check to unique
		// Step 4: Save list task execute /update status task/ update old record
		// Step 5: Add notification

		Long nodeId = dto.getNodeId();
		List<TaskExecute> nList = dto.getTaskExecutes();
		TaskHistory taskHistory = dto.getTaskHistory();

		// Step 1
		dto.valids(taskId, true);
		this.setTaskExeByOrgId(nList);

		// Task data validate
		Task task = taskService.valid(taskId, Message.TASK_NOT_FOUND);
		Long userId = BussinessCommon.getUserId();
		TaskExecute old = findByTaskIdAndUserId(taskId, userId);
		if (old == null || !checkStatusHandleCanTransfer(old.getStatus()))
			throw new RestExceptionHandler(Message.NO_TRANSFER_TASK);

		// Step 2
		if (taskHistory != null) {
			taskHistory.setTaskId(taskId);
			taskHistory = taskHistoryService.save(taskHistory);
		}

		// Step 3
		int step = old.getStep() == null ? Constant.START_STEP : old.getStep() + 1;
		List<TaskExecute> oList = findByTaskIdAndStep(taskId, step);
		Map<Long, TaskExecute> map = new HashMap<>();
		oList.forEach(i -> {
			map.put(i.getUserId(), i);
		});

		Set<Long> pushed = new HashSet<>();
		List<TaskExecute> rs = new ArrayList<>();
		for (TaskExecute i : nList) {
			Long key = i.getUserId();
			if (map.containsKey(key)) {
				i = map.get(key);
				i.setActive(true);
			}

			if (taskHistory != null) {
				i.setTaskHistoryId(taskHistory.getId());
			}

			i.set(taskId, userId, step, nodeId, com.vz.backend.business.util.Constant.TASK_EXCUTE_STATUS_NEW);

			// To check duplication
			if (!pushed.contains(key)) {
				rs.add(i);
				pushed.add(key);
			}
		}

		// Step 4
		// try-catch to check valid data consist of userId, groupId, orgId
		try {
			// save task exe
			rs = taskRepository.saveAll(rs);

			// update status (main role is allow)
			if (com.vz.backend.business.util.Constant.TASK_STATUS_INPROCESS != task.getStatus()
					&& Boolean.TRUE.equals(old.getIsExcute())) {
				task.setStatus(com.vz.backend.business.util.Constant.TASK_STATUS_INPROCESS);
				taskService.save(task);
			}

			// update old record
			old.setStatus(com.vz.backend.business.util.Constant.TASK_EXCUTE_STATUS_COMPLETE);
			old.setNextNode(nodeId);
			taskRepository.save(old);
			
			// Step 5
			this.addNotification(rs, task);
		} catch (Exception e) {
			e.printStackTrace();
			this.rollBack(taskHistory, rs);
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}

		return rs;
	}

	/**
	 * Set task execute by orgId
	 * 
	 * @param nList
	 */
	private void setTaskExeByOrgId(List<TaskExecute> nList) {
		// Set userId when choose orgId
		List<Long> orgIds = nList.stream().filter(i -> i.getOrgId() != null).map(TaskExecute::getOrgId).distinct()
				.collect(Collectors.toList());
		if (orgIds.isEmpty())
			return;
		Map<Long, Long> leadOrgMap = userService.leadByOrgIdsMap(orgIds);
		if (leadOrgMap.isEmpty()) {
			throw new RestExceptionHandler(Message.ORG_NO_LEAD_TRANSFER_TASK);
		}

		for (TaskExecute i : nList) {
			if (i.getType() != com.vz.backend.core.config.Constant.ORG)
				continue;

			Long key = i.getOrgId();
			if (leadOrgMap.containsKey(key)) {
				if (leadOrgMap.get(key) == null)
					throw new RestExceptionHandler(Message.ORG_NO_LEAD_TRANSFER_TASK);
				i.setUserId(leadOrgMap.get(key));
			}
		}
	}

	/**
	 * Add notification for executer
	 * 
	 * @param rs
	 * @param task
	 */
	private void addNotification(List<TaskExecute> rs, Task task) {
		Long taskId = task.getId();
		List<Long> userIdSupports = rs.stream().filter(i -> Boolean.TRUE.equals(i.getIsCombination()))
				.map(TaskExecute::getUserId).collect(Collectors.toList());
		List<Long> userIdMains = rs.stream().filter(i -> Boolean.TRUE.equals(i.getIsExcute()))
				.map(TaskExecute::getUserId).collect(Collectors.toList());
		noticeService.addAll(userIdMains, taskId, task.getTaskName(), DocumentTypeEnum.GIAO_VIEC,
				NotificationHandleStatusEnum.CV_XU_LY_CHINH, ModuleCodeEnum.TASK_MAIN);
		noticeService.addAll(userIdSupports, taskId, task.getTaskName(), DocumentTypeEnum.GIAO_VIEC,
				NotificationHandleStatusEnum.CV_PHOI_HOP, ModuleCodeEnum.TASK_SUPPORT);
		
		rs.forEach(i -> {
			objReadService.setRead(i.getUserId(), taskId, DocumentTypeEnum.GIAO_VIEC, false);
		});
	}

	private void rollBack(TaskHistory taskHistory, List<TaskExecute> rs) {
		if (taskHistory != null && taskHistory.getId() != null) {
			taskHistoryService.delete(taskHistory);
		}

		if (BussinessCommon.isEmptyList(rs)) {
			rs.forEach(i -> i.setActive(false));
		}
	}

	/**
	 * Find last step of userId
	 * 
	 * @param taskId
	 * @param userId
	 * @return
	 */
	public TaskExecute findByTaskIdAndUserId(Long taskId, Long userId) {
		List<TaskExecute> taskExe = taskRepository.findByTaskIdAndUserId(taskId, userId, BussinessCommon.getClientId());
		if (taskExe.isEmpty())
			return null;
		return taskExe.get(0);
	}

	/**
	 * Find by step
	 * 
	 * @param taskId
	 * @param step
	 * @return
	 */
	private List<TaskExecute> findByTaskIdAndStep(Long taskId, int step) {
		return taskRepository.findByTaskIdAndStepAndClientId(taskId, step, BussinessCommon.getClientId());
	}

	/**
	 * Save new task exe
	 * 
	 * @param taskId
	 * @param userId
	 * @return
	 */
	public TaskExecute save(Long taskId, Long userId) {
		TaskExecute tx = new TaskExecute();
		tx.setIsExcute(true);
		tx.setUserId(userId);
		tx.set(taskId, userId, Constant.START_STEP, Constant.START_NODE,
				com.vz.backend.business.util.Constant.TASK_EXCUTE_STATUS_INPROCESS);
		return taskRepository.save(tx);
	}

	/**
	 * Thông tin gửi nhận
	 * @param taskId
	 * @return
	 */
	public List<TaskExecute> getAll(Long taskId) {
		List<TaskExecute> allList = findByTaskId(taskId);
		return allList;
	}
	public List<TaskExecuteTrackingDto> buildTree(Long taskId) {
		List<TaskExecute> allList = findByTaskId(taskId);
		List<TaskExecuteTrackingDto> tList = new TaskExecuteTrackingDto().cast(allList);
		List<TaskExecuteTrackingDto> parents = getParents(tList);
		this.map = setChildList(tList);
		pushTree(parents);
		this.map.clear();
		return parents;
	}
	@SuppressWarnings("rawtypes")
	private Map<LabelValueDto, List<TaskExecuteTrackingDto>> map = new HashMap<>();
	private void pushTree(List<TaskExecuteTrackingDto> parents) {
		if (parents.isEmpty() || map.isEmpty())
			return;
		for (TaskExecuteTrackingDto p : parents) {
			setSubList(p);
			pushTree(p.getChildren());
		}
	}

	private void setSubList(TaskExecuteTrackingDto p) {
		LabelValueDto key = new LabelValueDto(p.getStep()+ 1, p.getToUserId());
		if(map.isEmpty() || !map.containsKey(key)) {
			return;
		}
		p.setChildren(map.get(key));
		map.remove(key);
	}
	
	
	private Map<LabelValueDto, List<TaskExecuteTrackingDto>> setChildList(List<TaskExecuteTrackingDto> childs) {
		Map<LabelValueDto, List<TaskExecuteTrackingDto>> map = new HashMap<>();
		for (TaskExecuteTrackingDto i : childs) {
			LabelValueDto key = new LabelValueDto(i.getStep(), i.getFrUserId());
			List<TaskExecuteTrackingDto> value = new ArrayList<>();
			if (map.containsKey(key)) {
				value = map.get(key);
			}
			value.add(i);
			map.put(key, value);
		}
		return map;
	}

	private List<TaskExecuteTrackingDto> getParents(List<TaskExecuteTrackingDto> all) {
		List<TaskExecuteTrackingDto> rs = new ArrayList<>();

		// check for old data
		if (BussinessCommon.isEmptyList(all))
			return rs;
		for (TaskExecuteTrackingDto i : all) {
			if (i.getFrUserId() == null) {
				return rs;
			}
		}

		// handle data
		Map<Long, List<TaskExecuteTrackingDto>> entry = all.stream().filter(i -> i.getStep() == Constant.START_STEP)
				.collect(Collectors.groupingBy(TaskExecuteTrackingDto::getFrUserId));

		entry.forEach((k, v) -> rs.addAll(v));
		return rs;
	}
	
	/**
	 * Get task exe by last step
	 * @param taskId
	 * @return
	 */
	public List<TaskExecute> getLastStepByTaskId(Long taskId) {
		return taskRepository.getLastStepByTaskId(taskId, BussinessCommon.getClientId());
	}
	
	/**
	 * Get task exe by status
	 * @param taskId
	 * @return
	 */
	public List<TaskExecute> getByTaskIdAndClientId(Long taskId) {
		return taskRepository.getByTaskIdAndClientId(taskId, BussinessCommon.getClientId());
	}
	
	/**
	 * Button Giao việc bổ sung
	 * @param taskId
	 * @param dto
	 * @return
	 */
	public List<TaskExecute> addTransfer(Long taskId, TaskTransferDto dto) {
		// Target : add buffer record to task execute
		// Step 1: Validate data
		// Step 2: Save history task
		// Step 3: Check to unique
		// Step 4: Save list task execute /update status task/ update old record
		// Step 5: Add notification

		Long nodeId = dto.getNodeId();
		List<TaskExecute> nList = dto.getTaskExecutes();
		TaskHistory taskHistory = dto.getTaskHistory();

		// Step 1
		dto.valids(taskId, false);
		this.setTaskExeByOrgId(nList);

		// Task data validate
		Task task = taskService.valid(taskId, Message.TASK_NOT_FOUND);
		Long userId = BussinessCommon.getUserId();
		TaskExecute old = findByTaskIdAndUserId(taskId, userId);
		if (old == null || !checkStatusHandleCanAddTransfer(old.getStatus()))
			throw new RestExceptionHandler(Message.NO_TRANSFER_TASK);

		// Step 2
		if (taskHistory != null) {
			taskHistory.setTaskId(taskId);
			taskHistory = taskHistoryService.save(taskHistory);
		}

		// Step 3
		int step = old.getStep() == null ? Constant.START_STEP : old.getStep() + 1;
		List<TaskExecute> oList = findByTaskIdAndStep(taskId, step);
		Map<Long, TaskExecute> map = new HashMap<>();
		oList.forEach(i -> {
			map.put(i.getUserId(), i);
		});

		Set<Long> pushed = new HashSet<>();
		List<TaskExecute> rs = new ArrayList<>();
		for (TaskExecute i : nList) {
			Long key = i.getUserId();
			if (map.containsKey(key)) {
				i = map.get(key);
				i.setActive(true);
			}

			if (taskHistory != null) {
				i.setTaskHistoryId(taskHistory.getId());
			}

			i.set(taskId, userId, step, nodeId, com.vz.backend.business.util.Constant.TASK_EXCUTE_STATUS_NEW);

			// To check duplication
			if (!pushed.contains(key)) {
				rs.add(i);
				pushed.add(key);
			}
		}

		// Step 4
		// try-catch to check valid data consist of userId, groupId, orgId
		try {
			// save task exe
			rs = taskRepository.saveAll(rs);

			// update status (main role is allow)
			if (com.vz.backend.business.util.Constant.TASK_STATUS_INPROCESS != task.getStatus()
					&& Boolean.TRUE.equals(old.getIsExcute())) {
				task.setStatus(com.vz.backend.business.util.Constant.TASK_STATUS_INPROCESS);
				taskService.save(task);
			}

			// update old record
			old.setStatus(com.vz.backend.business.util.Constant.TASK_EXCUTE_STATUS_COMPLETE);
			old.setNextNode(nodeId);
			taskRepository.save(old);
			
			// Step 5
			this.addNotification(rs, task);
		} catch (Exception e) {
			e.printStackTrace();
			this.rollBack(taskHistory, rs);
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}

		return rs;
	}

	public TaskExecute findBymaxStepAndtaskId(Long taskId) {
		return taskRepository.findBymaxStepAndtaskId(taskId, BussinessCommon.getClientId(),BussinessCommon.getUserId());
	}

	public List<TaskExecute> sizetaskPh(Long taskId) {
		return taskRepository.sizetaskPh(taskId, BussinessCommon.getClientId());
	}
}
