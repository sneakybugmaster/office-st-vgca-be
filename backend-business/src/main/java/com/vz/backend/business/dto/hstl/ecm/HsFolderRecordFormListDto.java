package com.vz.backend.business.dto.hstl.ecm;

import com.vz.backend.business.domain.hstl.HsFolderRecordForm;
import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;

@Getter
public class HsFolderRecordFormListDto {
	private Long id;
	private String name;
	private String status;
	private String src;
	private String address;
	private String date;
	private String createBy;
	private String createDate;

	public HsFolderRecordFormListDto(HsFolderRecordForm form, boolean tmp) {
		this.id = form.getId();
		this.name = form.getName();
		this.status = form.getStatusStr();
		this.src = form.getSrc() == null ? "" : form.getSrc().name;
		this.address = form.getAddress();
		this.date = DateTimeUtils.convertDateToStringPattern(form.getDate(), DateTimeUtils.DD_MM_YYYY);
		this.createBy = form.getCreatorName();
		this.createDate = DateTimeUtils.convertDateToStringPattern(form.getCreateDate(), DateTimeUtils.DD_MM_YYYY);
	}
}
