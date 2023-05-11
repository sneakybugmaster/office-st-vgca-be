package com.vz.backend.business.dto;

import java.util.Date;

import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class WordEditorDto {
	private Long id;
	private Long createBy;
	private String name;
	private Date startDate;
	private Date endDate;
	private DocumentStatusEnum status;
	private DocumentInHandleStatusEnum handleStatus;
	private String category;

	public String getHandleStatus() {
		if (DocumentStatusEnum.DONE.equals(status) && DocumentInHandleStatusEnum.DA_XU_LY.equals(handleStatus)) {
			return "Hoàn thành";
		}

		return this.handleStatus != null ? this.handleStatus.getName() : "";
	}

	public String getStatus() {
		return this.status != null ? this.status.getName() : "";
	}
}
