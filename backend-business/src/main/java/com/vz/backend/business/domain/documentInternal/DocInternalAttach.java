package com.vz.backend.business.domain.documentInternal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.domain.AttachmentBase;
import com.vz.backend.core.config.AttachmentTypeEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "DOC_INTERNAL_ATTACH", schema = "vz")
@Getter
@Setter
@NoArgsConstructor
public class DocInternalAttach extends AttachmentBase {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "doc_id")
	private Long docId;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "attachment_type")
	private AttachmentTypeEnum attachType; // File văn bản - File phụ lục
	
	@Column(name = "doc_internal_comment_id")
	private long docInternalCommentId;
	
	public DocInternalAttach(MultipartFile file, Long objId, String name) {
		super(file);
		this.docInternalCommentId = objId;
		super.setName(name);
	}

	public DocInternalAttach(String name, String type, Long size, AttachmentTypeEnum attachType) {
		super(name, type, size);
		this.attachType = attachType;
	}
	
	public DocInternalAttach(MultipartFile file, Long objId, String name, AttachmentTypeEnum attachType,
			boolean isComment) {
		super(file);
		if (Boolean.TRUE.equals(isComment)) {
			this.docInternalCommentId = objId;
		} else {
			this.docId = objId;
		}
		super.setName(name);
		this.attachType = attachType;
		super.setEncrypt(true);
	}
}
