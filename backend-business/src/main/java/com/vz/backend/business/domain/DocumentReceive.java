package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.domain.BaseModel;

import lombok.*;

@Entity
@Table(name = "DOCUMENT_RECEIVE", schema = "vz")
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class DocumentReceive extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "receive_id")
	private Long receiveId;

	@Column(name = "type")
	private String type;

	@Column(name = "doc_id")
	private Long docId;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "status")
	private DocumentStatusEnum status; // Hoàn thành sau khi ban hành

	@Transient
	private Long userId;

	@Transient
	private Long orgId;

	public DocumentReceive(Long id, Long receiveId, String type) {
		this.setId(id);
		this.receiveId = receiveId;
		this.type = type;
	}

	@Enumerated(EnumType.STRING)
	@Column(name = "handle_type")
	private HandleTypeEnum handleType;
	
	@Transient
	private String orgName;

	@Transient
	private String fullName;

	@Transient
	private String positionName;

	@Transient
	private String name;

	public DocumentReceive(Long receiveId, Long docId, String type, HandleTypeEnum handleType) {
		super();
		this.receiveId = receiveId;
		this.type = type;
		this.docId = docId;
		this.handleType = handleType;
	}

	public DocumentReceive(Long docId, Long receiveId, DocumentStatusEnum status) {
		super();
		this.docId = docId;
		this.receiveId = receiveId;
		this.status = status;
	}
		
}
