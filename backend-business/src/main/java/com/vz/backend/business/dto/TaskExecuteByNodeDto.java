package com.vz.backend.business.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class TaskExecuteByNodeDto {
	private Long userId;
	private Long orgId;
	private Long groupId;
	private Long nodeId;
	private Integer type;
}
