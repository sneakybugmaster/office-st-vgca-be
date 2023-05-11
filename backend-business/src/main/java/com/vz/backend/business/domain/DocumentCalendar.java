package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.*;

@Entity
@Table(name = "DOCUMENT_CALENDAR", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "c_id", "doc_in_id", "doc_out_id", "task_id", "type", "client_id" }) })
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class DocumentCalendar extends BaseModel {

	@JsonProperty(value = "cId")
	@Column(name = "c_id")
	private Long cId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_in_id", insertable = false, updatable = false)
	private Calendar2 calendar;
	
	@Column(name = "doc_in_id")
	private Long docInId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_in_id", insertable = false, updatable = false)
	private Documents docIn;
	
	@Column(name = "doc_out_id")
	private Long docOutId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "doc_out_id", insertable = false, updatable = false)
	private DocumentOut docOut;
	
	@Column(name = "task_id")
	private Long taskId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "task_id", insertable = false, updatable = false)
	private Task task;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "type")
	private DocumentTypeEnum type;
	
	@Transient
	private Long meetingId;

	private String numberOrSign;

	private String docTypeName;

	private String preview;

	public DocumentCalendar(Calendar2 calendar, Long objId, DocumentTypeEnum type) {
		super();
		this.calendar = calendar;
		this.type = type;
		if (objId == null)
			throw new RestExceptionHandler(Message.CALENDAR_DATA_INVALD);
		if (DocumentTypeEnum.VAN_BAN_DEN.equals(type)) {
			this.docInId = objId;
		} else if (DocumentTypeEnum.VAN_BAN_DI.equals(type)) {
			this.docOutId = objId;
		} else {
			this.taskId = objId;
		}
	}
	
	public String getDocInName() {
		return this.docIn != null ? this.docIn.getPreview() : "";
	}
	
	public String getDocOutName() {
		return this.docOut != null ? this.docOut.getPreview() : "";
	}
	
	public String getTaskName() {
		return this.task != null ? this.task.getTaskName() : "";
	}
	
	public String getTaskDescription() {
		return this.task != null ? this.task.getDescription() : "";
	}

	public DocumentCalendar(Long meetingId, Long docInId, Documents docIn, Long docOutId, DocumentOut docOut, DocumentTypeEnum type) {
		super();
		if(DocumentTypeEnum.VAN_BAN_DEN.equals(type)) {
			this.docInId = docInId;
			this.docIn = docIn;
		} 
		if(DocumentTypeEnum.VAN_BAN_DI.equals(type)) {
			this.docOutId = docOutId;
			this.docOut = docOut;
		} 
		this.meetingId = meetingId;
		this.type = type;
	}

	public String getNumberOrSign() {
		if (type.equals(DocumentTypeEnum.VAN_BAN_DI) && docOut != null) {
			return docOut.getNumberOrSign();
		} else if (type.equals(DocumentTypeEnum.VAN_BAN_DEN) && docIn != null) {
			return docIn.getNumberOrSign();
		} else
			return numberOrSign;
	}

	public String getDocTypeName() {
		if (type.equals(DocumentTypeEnum.VAN_BAN_DI)) {
			return DocumentTypeEnum.VAN_BAN_DI.getName();
		} else if (type.equals(DocumentTypeEnum.VAN_BAN_DEN)) {
			return DocumentTypeEnum.VAN_BAN_DEN.getName();
		} else
			return docTypeName;
	}

	public String getPreview() {
		if (type.equals(DocumentTypeEnum.VAN_BAN_DI) && docOut != null) {
			return docOut.getPreview();
		} else if (type.equals(DocumentTypeEnum.VAN_BAN_DEN) && docIn != null) {
			return docIn.getPreview();
		} else
			return "";
	}
	
	
}
