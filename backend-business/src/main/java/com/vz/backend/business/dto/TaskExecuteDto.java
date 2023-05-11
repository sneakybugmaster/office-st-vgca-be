package com.vz.backend.business.dto;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@Data
@NoArgsConstructor
public class TaskExecuteDto {
	private Long taskExeId;
	private String comment;
	private Long id;
	private String taskName;
	private Date startDate;
	private Date endDate;
	private Integer progress;
	private Boolean important;
	private String codeTask;
	private String userAssignName;
	private Integer taskCombinationStatus; // status task exe
	private Integer approveStatus; // approve task
	private Integer status; // status task
	private Boolean read;
}
