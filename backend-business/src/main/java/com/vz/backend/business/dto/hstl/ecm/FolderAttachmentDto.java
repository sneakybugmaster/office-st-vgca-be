package com.vz.backend.business.dto.hstl.ecm;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.domain.Attachment;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.business.domain.hstl.HsFolderFile;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FolderAttachmentDto {
	private String tenFile;
	private String urlFile;
	private String ngayTao;
	private String kichThuocFile;
	private String dinhDangFile;
	@JsonIgnore
	private Long folderId;
	
	@JsonIgnore
	private String downloadLink = "/api/hstl-form/download/2/";

	public FolderAttachmentDto(TaskAttachment t) {
		this.tenFile = t.getDisplayName();
		this.urlFile = BussinessCommon.getDomain() + downloadLink + t.getName();
		this.ngayTao = DateTimeUtils.convertDateToStringPattern(t.getCreateDate(), DateTimeUtils.YYYY_MM_DD);
		this.kichThuocFile = t.getSize() == null ? "0" : t.getSize().toString();
		this.dinhDangFile = t.getType();
	}

	public FolderAttachmentDto(HsFolderFile t, Boolean tmp) {
		this.tenFile = t.getDisplayName();
		this.urlFile = BussinessCommon.getDomain() + "/api/hstl-form/download/1/" + t.getFileName();
		this.ngayTao = DateTimeUtils.convertDateToStringPattern(t.getCreateDate(), DateTimeUtils.YYYY_MM_DD);
		this.kichThuocFile = t.getFileSize() == null ? "0" : t.getFileSize().toString();
		this.folderId = t.getFolderId();
		this.dinhDangFile = t.getFileType();
	}

	public FolderAttachmentDto(Attachment t, Long folderId) {
		this.tenFile = t.getDisplayName();
		this.urlFile = BussinessCommon.getDomain() + downloadLink + t.getName();
		this.ngayTao = DateTimeUtils.convertDateToStringPattern(t.getCreateDate(), DateTimeUtils.YYYY_MM_DD);
		this.kichThuocFile = t.getSize() == null ? "0" : t.getSize().toString();
		this.folderId = folderId;
		this.dinhDangFile = t.getType();
	}
	
	public FolderAttachmentDto(DocumentOutAttachment t, Long folderId) {
		this.tenFile = t.getDisplayName();
		this.urlFile = BussinessCommon.getDomain() + downloadLink + t.getName();
		this.ngayTao = DateTimeUtils.convertDateToStringPattern(t.getCreateDate(), DateTimeUtils.YYYY_MM_DD);
		this.kichThuocFile = t.getSize() == null ? "0" : t.getSize().toString();
		this.folderId = folderId;
		this.dinhDangFile = t.getType();
	}
}
