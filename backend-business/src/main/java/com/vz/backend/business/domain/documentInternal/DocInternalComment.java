package com.vz.backend.business.domain.documentInternal;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.vz.backend.business.config.DocInternalApproveStatusEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "DOC_INTERNAL_COMMENT", schema = "vz")
@Getter
@Setter
@NoArgsConstructor
public class DocInternalComment extends BaseModel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "approve_id")
	private Long approveId;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "handle_status")
	private DocInternalApproveStatusEnum handleStatus;
	
	@Column(name = "comment", columnDefinition = "TEXT")
	private String comment;
	
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "create_by", updatable = false, insertable = false)
	private User createUser;

	public DocInternalComment(Long approveId, String comment, DocInternalApproveStatusEnum handleStatus) {
		super();
		this.approveId = approveId;
		this.comment = comment;
		this.handleStatus = handleStatus;
	}
	
}
