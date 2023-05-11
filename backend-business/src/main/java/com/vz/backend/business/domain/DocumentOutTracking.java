package com.vz.backend.business.domain;

import java.util.Date;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.auth.SecurityContext;
import com.vz.backend.core.config.DocumentOutTrackingEnum;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "SYS_DOCUMENT_OUT_TRACKING", schema = "vz", indexes = {@Index(name = "INDEX_DOCUMENT_OUT_TRACKING",columnList = "id,doc_id,from_user_id,to_user_id,handler_id,action")})
@JsonIgnoreProperties(value = { "handlerId", "fromUserId", "toUserId" }, allowGetters = true)
public class DocumentOutTracking {
	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private Long id;

	@Column(name = "client_id")
	private Long clientId;

	@Column(name = "doc_id")
	private Long docId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_id", insertable = false, updatable = false)
	private DocumentOut doc;

	@Column(name = "org_name")
	private String orgName;

	@Column(name = "user_name")
	private String userName;

	@Enumerated(EnumType.STRING)
	@Column(name = "action")
	private DocumentOutTrackingEnum action;

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

	// Người xử lý
	@Column(name = "handler_id")
	private Long handlerId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "handler_id", updatable = false, insertable = false)
	private User handler;

	@Column(name = "comment_id")
	private Long commentId;

	@Column(name = "pre_node_id")
	private Long preNodeId;

	@Column(name = "create_date")
	private Date createDate;

	@Column(name = "update_date")
	private Date updateDate;
	
	@Column(name = "file_name")
	private String fileName;

	@PrePersist
	public void prePersist() {
		this.createDate = new Date();
		this.updateDate = this.createDate;
		User user = SecurityContext.getCurrentUser();
		if (user != null) {
			this.userName = user.getUserName();
			this.clientId = user.getClientId();
			if (this.fromUserId == null) {
				this.fromUserId = user.getId();
			}
		}
	}

	@PreUpdate
	public void preUpdate() {
		this.updateDate = new Date();
	}

	public DocumentOutTracking(Long docId, Long fromUserId, Long toUserId, DocumentOutTrackingEnum action, String orgName, Long handlerId, Long cmtId) {
		super();
		this.docId = docId;
		this.action = action;
		this.fromUserId = fromUserId;
		this.toUserId = toUserId;
		this.orgName = orgName;
		this.handlerId = handlerId;
		this.commentId = cmtId;
	}
	
}
