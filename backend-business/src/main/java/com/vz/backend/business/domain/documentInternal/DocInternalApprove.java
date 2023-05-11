package com.vz.backend.business.domain.documentInternal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.config.DocInternalApproveStatusEnum;
import com.vz.backend.business.config.DocInternalApproveTypeEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Getter
@Setter
@NoArgsConstructor
@Table(name = "DOC_INTERNAL_APPROVE", schema = "vz")
public class DocInternalApprove extends BaseModel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "doc_id")
	private Long docId;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "type")
	private DocInternalApproveTypeEnum type;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "handle_status")
	private DocInternalApproveStatusEnum handleStatus;
	
	@Column(name = "user_id")
	private Long userId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", updatable = false, insertable = false)
	private User user;
	
	@Column(name = "org_id")
	private Long orgId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "org_id", updatable = false, insertable = false)
	private Organization org;
	
	@Column(name = "last_comment")
	private String lastComment;
	
	@PrePersist
	public void prePersist() {
	}

	@PreUpdate
	public void preUpdate() {
	}

	public DocInternalApprove(Long docId, DocInternalApproveTypeEnum type, Long id, DocInternalApproveStatusEnum handleStatus) {
		super();
		this.docId = docId;
		this.type = type;
		if (DocInternalApproveTypeEnum.ORG.equals(type))
			this.orgId = id;
		else 
			this.userId = id;
		this.handleStatus = handleStatus;
	}
	
	public DocInternalApprove(Long docId, Long id, boolean user) {
		super();
		this.docId = docId;
		this.type = DocInternalApproveTypeEnum.RECEIVER;
		if (Boolean.FALSE.equals(user))
			this.orgId = id;
		else 
			this.userId = id;
		this.handleStatus = DocInternalApproveStatusEnum.DA_DUYET;
	}

	
}
