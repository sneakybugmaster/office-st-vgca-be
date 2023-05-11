package com.vz.backend.business.dto;

import com.vz.backend.business.domain.Documents;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;

import lombok.AllArgsConstructor;
import lombok.Data;

@AllArgsConstructor
@Data
public class DocumentInManipulationDto {
	private Documents doc;
	private String toUsers;
	private String frUsers;
	private DocumentInHandleStatusEnum handleStatus;
}
