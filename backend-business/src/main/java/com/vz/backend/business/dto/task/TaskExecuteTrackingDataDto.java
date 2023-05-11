package com.vz.backend.business.dto.task;

import lombok.Getter;

@Getter
public class TaskExecuteTrackingDataDto {
	/**
	 * Assigner
	 */
	private String frUserFullName;

	/**
	 * Executor
	 */
	private String toUserFullName;
	/**
	 * Handle type
	 */
	private String typeName;

	/**
	 * Handle status
	 */
	private String statusName;

	/**
	 * Content task
	 */
	private String comment;

	/**
	 * Progress task
	 */
	private Integer progress;
	
	public TaskExecuteTrackingDataDto(TaskExecuteTrackingDto dto) {
		super();
		this.frUserFullName = dto.getFrUserFullName();
		this.toUserFullName = dto.getToUserFullName();
		this.typeName = dto.getTypeName();
		this.statusName = dto.getStatusName();
		this.comment = dto.getComment();
		this.progress = dto.getProgress();
	}
}
