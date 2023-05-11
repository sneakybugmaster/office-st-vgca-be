package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "DOCUMENT_OUT_ATTACH_VERSION", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor

public class AttachmentVersion extends AttachmentBase {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "origin_name")
	private String originName;

	@Column(name = "doc_id")
	private Long docId;

	@Column(name = "user_full_name")
	private String userFullName;

	@Column(name = "file_version")
	private int version;
}
