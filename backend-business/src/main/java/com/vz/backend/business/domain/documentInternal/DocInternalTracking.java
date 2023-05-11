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
import com.vz.backend.business.config.DocInternalTrackingEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Getter
@Setter
@NoArgsConstructor
@Table(name = "DOC_INTERNAL_TRACKING", schema = "vz")
public class DocInternalTracking extends BaseModel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "doc_id")
	private Long docId;

	@Column(name = "from_user_id")
	private Long fromUserId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "from_user_id", updatable = false, insertable = false)
	private User fromUser;

	@Column(name = "to_user_id")
	private Long toUserId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "to_user_id", updatable = false, insertable = false)
	private User toUser;

	@Enumerated(EnumType.STRING)
	@Column(name = "action")
	private DocInternalTrackingEnum action;
	
	@Column(name = "comment_id")
	private Long commentId;

	@PrePersist
	public void prePersist() {
	}

	@PreUpdate
	public void preUpdate() {
	}

	public DocInternalTracking(Long docId, Long fromUserId, DocInternalTrackingEnum action) {
		super();
		this.docId = docId;
		this.fromUserId = fromUserId;
		this.action = action;
	}
	
	public DocInternalTracking(Long docId, Long fromUserId, DocInternalTrackingEnum action, Long commentId) {
		this(docId, fromUserId, action);
		this.commentId = commentId;
	}
}
