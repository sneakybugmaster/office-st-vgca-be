package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Table;

import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.core.config.AttachmentTypeEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "DOCUMENT_OUT_ATTACH", schema = "vz", indexes = {@Index(name = "DOC_OUT_INX_ATTACH",columnList = "id,attachment_type,doc_id")})
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DocumentOutAttachment extends AttachmentBase {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "attachment_type")
	private AttachmentTypeEnum attachmentType; // Dự thảo - VB liên quan - CommentAttach

	@Column(name = "doc_id")
	private Long docId;

	@Column(name = "comment_id")
	private Long cmtId;

	@Column(name = "file_changed")
	private Boolean isChanged;

	public String toFolder() {
		switch (attachmentType) {
		case DRAFT:
			return "Dự thảo";
		case RELATE:
			return "Liên quan";
		default:
			return "";
		}
	}

	public DocumentOutAttachment(String name, String type, AttachmentTypeEnum attachmentType, Long size) {
		super(name, type, size);
		this.attachmentType = attachmentType;
	}
	
	public DocumentOutAttachment(MultipartFile file, Long objId, String name, AttachmentTypeEnum attachmentType) {
		super(file);
		super.setName(name);
		this.attachmentType = attachmentType;
		if (AttachmentTypeEnum.COMMENT.equals(attachmentType)) {
			this.cmtId = objId;
		} else {
			this.docId = objId;
		}
		super.setEncrypt(true);
	}
}
