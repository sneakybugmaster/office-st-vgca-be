package com.vz.backend.business.domain;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.config.DocumentInTrackingEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "SYS_DOCUMENT_IN_TRACKING", schema = "vz", indexes = {@Index(name = "DOC_IN_TRACKING_INX_ABX",columnList = "id,doc_id,user_id,action")})
public class DocumentInTracking extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "doc_id")
	private Long docId;

	@Column(name = "user_id")
	private Long userId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", updatable = false, insertable = false)
	private User user;

	@Enumerated(EnumType.STRING)
	@Column(name = "action")
	private DocumentInTrackingEnum action;

	@Column(name = "org_name")
	private String orgName;

	@Column(name = "doc_type")
	private String docType;

	public DocumentInTracking(Long docId, Long userId, DocumentInTrackingEnum action, String orgName, String docType) {
		this.docId = docId;
		this.userId = userId;
		this.action = action;
		this.orgName = orgName;
		this.docType = docType;
	}
}
