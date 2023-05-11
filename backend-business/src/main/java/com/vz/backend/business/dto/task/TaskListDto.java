package com.vz.backend.business.dto.task;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.config.ButtonStatusEnum;
import com.vz.backend.business.domain.Task;
import com.vz.backend.core.common.BussinessCommon;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class TaskListDto {

	public TaskListDto(Long id, String userExcutePrimaryName, String orgName) {
		super();
		this.id = id;
		this.userExcutePrimaryName = userExcutePrimaryName;
		this.orgName = orgName;
	}

	/**
	 * Mã định danh công việc
	 */
	private Long id;

	/**
	 * Tên công việc
	 */
	private String taskName;

	/**
	 * Mã công việc
	 */
	private String codeTask;

	/**
	 * Người giao
	 */
	private String userAssignName;

	/**
	 * Trạng thái công việc
	 */
	private String statusName;

	/**
	 * Ngày bắt đầu
	 */
	private Date startDate;

	/**
	 * Ngày kết thúc
	 */
	private Date endDate;

	/**
	 * Tiến độ
	 */
	private Long progress;

	/**
	 * Công việc quan trọng
	 */
	private Boolean important;

	/**
	 * Node
	 */
	private Long nodeId;

	/**
	 * Danh sách button
	 */
	private TaskButtonActionDto button;

	@JsonIgnore
	private Integer taskStatus;

	@JsonIgnore
	private Long createBy;

	@JsonIgnore
	private Integer handleStatus;

	@JsonIgnore
	private Boolean lastNode;

	@JsonIgnore
	private Boolean hasNextNode;

	/**
	 * Tại node hiện tại có cấu hình check đánh giá
	 */
	@JsonIgnore
	private Boolean reviewRequired;
	
	/**
	 * Kết quả đánh giá của node trước
	 */
	@JsonIgnore
	private Boolean reviewRequiredResult;
	
	/**
	 * Người xử lý chính (nếu có)
	 */
	private String userExcutePrimaryName;
	
	/**
	 * Xuất file
	 */
	private String priorityName;
	
	/**
	 * Flag mark user close task
	 */
	private Boolean close = false;
	
	/**
	 * Tổ chức thực hiện
	 */
	private String orgName;
	
	/**
	 * Flag mark read
	 */
	private Boolean read;
	
	private Long nextNode;

	public TaskListDto(Task task, Integer handleStatus, Long node, Boolean reviewRequiredResult, Boolean important, Boolean close, Long nextNode) {
		this.id = task.getId();
		this.taskName = task.getTaskName();
		this.codeTask = task.getCodeTask();
		this.userAssignName = task.getUserAssignName();
		this.statusName = BussinessCommon.getStatusName(handleStatus);
		this.startDate = task.getStartDate();
		this.endDate = task.getEndDate();
		this.progress = task.getProgress();
		this.important = important;
		this.taskStatus = task.getStatus();
		this.createBy = task.getCreateBy();
		this.handleStatus = handleStatus;
		this.nodeId = node;
		this.reviewRequiredResult= reviewRequiredResult;
		this.button = new TaskButtonActionDto(ButtonStatusEnum.HIDDEN);
		this.close = close;
		this.nextNode = nextNode;
	}

	public TaskListDto(Task task, String userExcutePrimaryName) {
		this.id = task.getId();
		this.taskName = task.getTaskName();
		this.codeTask = task.getCodeTask();
		this.userAssignName = task.getUserAssignName();
		this.statusName = BussinessCommon.getStatusName(task.getStatus());
		this.startDate = task.getStartDate();
		this.endDate = task.getEndDate();
		this.progress = task.getProgress();
		this.taskStatus = task.getStatus();
		this.createBy = task.getCreateBy();
		this.button = new TaskButtonActionDto(ButtonStatusEnum.HIDDEN);
		this.userExcutePrimaryName = userExcutePrimaryName;
		this.priorityName = task.getPriorityName();
	}

	public TaskListDto(Task task, Integer handleStatus, Long node, Boolean reviewRequiredResult, Boolean important, Boolean close, Long nextNode,Integer progress) {
		this.id = task.getId();
		this.taskName = task.getTaskName();
		this.codeTask = task.getCodeTask();
		this.userAssignName = task.getUserAssignName();
		this.statusName = BussinessCommon.getStatusName(handleStatus);
		this.startDate = task.getStartDate();
		this.endDate = task.getEndDate();
		this.progress = progress != null ? Long.valueOf(progress) : 0L;
		this.important = important;
		this.taskStatus = task.getStatus();
		this.createBy = task.getCreateBy();
		this.handleStatus = handleStatus;
		this.nodeId = node;
		this.reviewRequiredResult= reviewRequiredResult;
		this.button = new TaskButtonActionDto(ButtonStatusEnum.HIDDEN);
		this.close = close;
		this.nextNode = nextNode;
	}

}
