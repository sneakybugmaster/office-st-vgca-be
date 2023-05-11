package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "SYS_DOCUMENT_IN_MANIPULATION", schema = "vz")
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "client", "clientId", "active"})
public class DocumentInManipulation extends BaseModel {
	private static final long serialVersionUID = 1L;
	@Column(name = "doc_id")
	private Long docId;
//	@JsonIgnore
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "doc_id", updatable = false, insertable = false)
	private Documents doc;
	
	@Column(name = "to_user")
	private Long toUser;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "to_user", updatable = false, insertable = false)
	private User toUsers;

	@Column(name = "fr_user")
	private Long frUser;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "fr_user", updatable = false, insertable = false)
	private User frUsers;

	@Enumerated(EnumType.STRING)
	@Column(name = "handle_status")
	private DocumentInHandleStatusEnum handleStatus;
	
	@PrePersist
	public void prePersit() {
		User user = BussinessCommon.getUser();
		if(user != null) this.frUser = user.getId();
	}

	public DocumentInManipulation(Long docId, Long toUser, DocumentInHandleStatusEnum handleStatus) {
		this.docId = docId;
		this.toUser = toUser;
		this.handleStatus = handleStatus;
	}
	
	public String getHandleStatus() {
		return this.handleStatus != null ? handleStatus.getName() : "";
	}
	
	public String getToUserStr() {
		return this.toUsers != null ? toUsers.getFullName() : "";
	}
	
	public String getFrUserStr() {
		return this.frUsers != null ? frUsers.getFullName() : "";
	}
	
	@Transient
	private boolean read;
}
