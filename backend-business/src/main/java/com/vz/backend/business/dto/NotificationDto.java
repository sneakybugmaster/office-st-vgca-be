package com.vz.backend.business.dto;

import java.util.Date;

import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class NotificationDto {
	private Long id;
	private Long docId;
	private String description;
	private String docType;
	private NotificationHandleStatusEnum docStatus;
	private String docStatusName;
	private Boolean read;
	private Date date;
	private ModuleCodeEnum moduleCode;
	private String actor;
}
