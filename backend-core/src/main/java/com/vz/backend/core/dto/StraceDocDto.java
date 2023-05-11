package com.vz.backend.core.dto;

import org.springframework.data.domain.Page;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentStatusEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class StraceDocDto {
	private long no;
	private long countDoc;
	private long docType;
	private DocumentStatusEnum status;
	private String clientName;
	private String nameStatus;
	private String nameDocType;

	public StraceDocDto(long countDoc, long docType, DocumentStatusEnum status, String clientName, String nameDocType) {
		this.countDoc = countDoc;
		this.docType = docType;
		this.status = status;
		this.clientName = clientName;
		this.nameStatus = status.getName();
		this.nameDocType = nameDocType;
	}

	public static Page<StraceDocDto> convert(Page<StraceDocDto> pageRs) {
		if (!BussinessCommon.isEmptyPage(pageRs)) {
			long i = pageRs.getPageable().getOffset() + 1;
			for (StraceDocDto s : pageRs.getContent()) {
				s.setNo(i);
			}
		}
		return pageRs;
	}
}
