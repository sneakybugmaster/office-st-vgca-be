package com.vz.backend.business.dto;

import java.io.Serializable;

import com.vz.backend.business.domain.Task;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class TaskDto implements Serializable {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	Long id;
	String taskName;
	String userAssignName;
	public TaskDto(Task task) {
		super();
		this.id = task.getId();
		this.taskName = task.getTaskName();
		this.userAssignName = task.getUserAssign().getFullName();
	}
}
