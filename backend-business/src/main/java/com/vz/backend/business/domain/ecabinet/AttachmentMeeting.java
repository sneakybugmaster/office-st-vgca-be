package com.vz.backend.business.domain.ecabinet;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.config.ecabinet.FileTypeEnum;
import com.vz.backend.business.domain.AttachmentBase;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "ATTACHMENT_MEETING", schema = "vz")
@NoArgsConstructor
@AllArgsConstructor
@Data
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
public class AttachmentMeeting extends AttachmentBase {

	/*
	 * Phân loại file
	 */
	@Column(name = "file_type")
	@Enumerated(EnumType.STRING)
	private FileTypeEnum fileType;

	@Column(name = "obj_id")
	private Long objId;

	@Column(name = "description", columnDefinition = "TEXT")
	private String description;

	public AttachmentMeeting(MultipartFile file, Long objId, String name, FileTypeEnum fileType) {
		super(file);
		super.setName(name);
		this.objId = objId;
		this.fileType = fileType;
	}

	/*
	 * Trạng thái file
	 */
	@Column(name = "file_status")
	@Enumerated(EnumType.STRING)
	private FileTypeEnum fileStatus;

	/*
	 * Xét duyệt file
	 */
	@Column(name = "file_approvement")
	@Enumerated(EnumType.STRING)
	private FileTypeEnum fileApprovement;

	/*
	 * Lý do trả lại
	 */
	private String reason;

	@Override
	public void valids() {
		if (FileTypeEnum.REJECT.equals(this.fileApprovement)) {
			BussinessCommon.require("Lý do", this.reason);
			BussinessCommon.validLengthData(this.reason, "Lý do", 500);
		} else if (!FileTypeEnum.REJECT.equals(this.fileApprovement) && !StringUtils.isEmpty(this.reason)) {
			throw new RestExceptionHandler("Không trả lại không nhập lý do");
		}
	}

	public AttachmentMeeting(MultipartFile file, String name, FileTypeEnum fileType, Long objId) {
		super(file);
		super.setName(name);
		this.fileType = fileType;
		this.objId = objId;
	}
	
	public AttachmentMeeting(String name, FileTypeEnum fileType, Long objId, String description, FileTypeEnum fileStatus) {
		super();
		super.setName(name);
		this.fileType = fileType;
		this.objId = objId;
		this.description = description;
		this.fileStatus = fileStatus;
	}
	
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "create_by", insertable = false, updatable = false)
	private User creator;
	
	public AttachmentMeeting(String name, FileTypeEnum fileType, Long objId, Long size, String type) {
		super.setName(name);
		super.setSize(size);
		super.setType(type);
		this.fileType = fileType;
		this.objId = objId;
	}

	public AttachmentMeeting(String name, Long size, String type, FileTypeEnum fileType, Long objId, String description, FileTypeEnum fileStatus) {
		super.setName(name);
		super.setSize(size);
		super.setType(type);
		this.fileType = fileType;
		this.objId = objId;
		this.description = description;
		this.fileStatus = fileStatus;
	}
}
