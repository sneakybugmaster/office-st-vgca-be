package com.vz.backend.business.dto.task;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.domain.TaskExecute;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.domain.User;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class TaskExecuteTrackingDto {
	/**
	 * Data from domain
	 */
	@JsonIgnore
	private Long txId;
	@JsonIgnore
	private Long frUserId;
	@JsonIgnore
	private Long toUserId;
	@JsonIgnore
	private Integer step;
	@JsonIgnore
	private Long node;

	/**
	 * Assigner
	 */
	@JsonIgnore
	private String frUserFullName;

	/**
	 * Executor
	 */
	@JsonIgnore
	private String toUserFullName;
	/**
	 * Handle type
	 */
	@JsonIgnore
	private String typeName;

	/**
	 * Handle status
	 */
	@JsonIgnore
	private String statusName;

	/**
	 * Content task
	 */
	@JsonIgnore
	private String comment;

	/**
	 * Progress task
	 */
	@JsonIgnore
	private Integer progress;
	
	public TaskExecuteTrackingDataDto getData() {
		return new TaskExecuteTrackingDataDto(this);
	}

	/**
	 * Children list
	 */
	private List<TaskExecuteTrackingDto> children;

	private TaskExecuteTrackingDto(TaskExecute taskExe) {
		this.txId = taskExe.getId();
		User fr = taskExe.getFrUser();
		User to = taskExe.getUser();
		this.frUserId = taskExe.getFrUserId();
		this.toUserId = taskExe.getUserId();
		this.frUserFullName = fr != null ? fr.getFullName() + " - " + fr.getOrgModel().getName() : "";
		this.toUserFullName = to != null ? to.getFullName() + " - " + to.getOrgModel().getName() : "";
		this.progress = taskExe.getProgress() == null ? 0 : taskExe.getProgress();
		this.comment = taskExe.getComment();
		this.node = taskExe.getNode() == null ? Constant.START_NODE : taskExe.getNode();
		this.step = taskExe.getStep() == null ? Constant.START_STEP : taskExe.getStep();
		this.statusName = BussinessCommon.getStatusName(taskExe.getStatus());
		this.typeName = Boolean.TRUE.equals(taskExe.getIsExcute()) ? "Xử lý chính" : "Xử lý phối hợp";
		this.children = new ArrayList<>();
	}

	/**
	 * Cast domain to dto
	 * 
	 * @param taskExe
	 * @return
	 */
	public List<TaskExecuteTrackingDto> cast(List<TaskExecute> taskExe) {
		List<TaskExecuteTrackingDto> rs = new ArrayList<>();
		if (BussinessCommon.isEmptyList(taskExe))
			return rs;

		for (TaskExecute i : taskExe) {
			rs.add(new TaskExecuteTrackingDto(i));
		}
		return rs;
	}
}
