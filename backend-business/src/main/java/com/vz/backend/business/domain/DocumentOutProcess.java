package com.vz.backend.business.domain;

import java.io.Serializable;

import javax.persistence.*;

import org.hibernate.annotations.NamedNativeQueries;
import org.hibernate.annotations.NamedNativeQuery;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.util.Constant;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@SqlResultSetMapping(name = "signerDocumentResult", classes = {
		@ConstructorResult(targetClass = com.vz.backend.business.dto.KnowableDto.class, columns = {
				@ColumnResult(name = "id"),
		})
})

@SqlResultSetMapping(name = "signerIdDocumentResult", classes = {
		@ConstructorResult(targetClass = com.vz.backend.core.dto.SignerDto.class, columns = {
				@ColumnResult(name = "id"),
				@ColumnResult(name = "fullName"),
				@ColumnResult(name = "orgName"),
				@ColumnResult(name = "position"),
				@ColumnResult(name = "phone")
		})
})
@NamedNativeQueries(value = {
		@NamedNativeQuery(name = "DocumentOutProcess.signer", query = Constant.SIGNER_DOCUMENT_OUT_QUERY, resultSetMapping = "signerDocumentResult"),
		@NamedNativeQuery(name = "DocumentOutProcess.signerId", query = Constant.SIGNER_NAME_DOCUMENT_OUT_QUERY, resultSetMapping = "signerIdDocumentResult"),})
@Data
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "SYS_DOCUMENT_OUT_PROCESS", schema = "vz", indexes = {@Index(name = "INDEX_DOCUMENT_OUT_PROCESS",columnList = "id,doc_id,user_id,delegate_user_id,delegate_id,handler_id")})
@JsonIgnoreProperties(value = { "id", "clientId", "createDate", "userId", "handleStatus", "delegateUserId",
		"delegateId" }, allowGetters = true)
public class DocumentOutProcess extends BaseModel implements Serializable {
	private static final long serialVersionUID = 1L;

	@Column(name = "doc_id")
	private Long docId;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_id", updatable = false, insertable = false)
	private DocumentOut documentOut;

	@Column(name = "org_name")
	private String orgName;

	@Column(name = "user_id")
	private Long userId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", updatable = false, insertable = false)
	private User user;
	// Người ủy quyền
	@Column(name = "delegate_user_id")
	private Long delegateUserId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "delegate_user_id", updatable = false, insertable = false)
	private User delegateUser;
	// Ủy quyền
	@Column(name = "delegate_id")
	private Long delegateId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "delegate_id", updatable = false, insertable = false)
	private Delegate delegate;
	// Người xử lý
	@Column(name = "handler_id")
	private Long handlerId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "handler_id", updatable = false, insertable = false)
	private User handler;

	@Enumerated(EnumType.STRING)
	@Column(name = "handle_type")
	private HandleTypeEnum handleType;

	@Enumerated(EnumType.STRING)
	@Column(name = "handle_status")
	private DocumentOutHandleStatusEnum handleStatus;

	@Column(name = "node_id")
	private Long nodeId;

	@Column(name = "[read]")
	private Boolean read;

	@PrePersist
	public void prePersit() {
		this.read = false;
	}
}
