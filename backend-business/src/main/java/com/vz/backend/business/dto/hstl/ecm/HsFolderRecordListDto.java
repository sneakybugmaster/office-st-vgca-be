package com.vz.backend.business.dto.hstl.ecm;

import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.business.domain.hstl.HsFolderRecord;
import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;

@Getter
public class HsFolderRecordListDto {
	private Long id;
	private String status;
	private String createBy;
	private String createDate;
	private String title;
	private String form;

	public HsFolderRecordListDto(HsFolderRecord h, boolean tmp) {
		this.id = h.getId();
		this.status = h.getStatusStr();
		this.createBy = h.getCreatorName();
		this.createDate = DateTimeUtils.convertDateToStringPattern(h.getCreateDate(), DateTimeUtils.DD_MM_YYYY);
		this.title = h.getHsFolder() == null ? "" : h.getHsFolder().getTitle();
		this.form = h.getForm() != null && h.getForm().getSrc() != null ? h.getForm().getSrc().name : "";
	}
	
	public HsFolderRecordListDto(HsFolder h, boolean tmp) {
		this.id = h.getId();
		this.status = "Gửi mới";
		this.createBy = h.getCreator();
		this.createDate = DateTimeUtils.convertDateToStringPattern(h.getCreateDate(), DateTimeUtils.DD_MM_YYYY);
		this.title = h.getTitle();
		this.form  = h.getFileCode();
	}
}
