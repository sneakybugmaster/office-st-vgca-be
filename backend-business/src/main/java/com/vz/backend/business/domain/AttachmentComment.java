package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import org.springframework.web.multipart.MultipartFile;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "ATTACHMENT_COMMENT", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AttachmentComment extends AttachmentBase {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	
	@Column(name = "comment_id")
	private long commentId;
	
	public AttachmentComment(MultipartFile file, Long objId, String name) {
		super(file);
		this.commentId = objId;
		super.setName(name);
		super.setEncrypt(true);
	}
}
