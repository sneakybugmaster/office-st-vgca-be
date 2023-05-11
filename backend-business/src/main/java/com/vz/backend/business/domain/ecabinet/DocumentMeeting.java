package com.vz.backend.business.domain.ecabinet;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "DOCUMENT_MEETING", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "meeting_id", "doc_in_id", "doc_out_id", "type", "client_id" }) })
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DocumentMeeting extends BaseModel{
	
	@Column(name = "meeting_id")
	private Long meetingId;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "meeting_id", updatable = false, insertable = false)
	@JsonIgnore
	private Meeting meeting;
	
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
	
	@Enumerated(EnumType.STRING)
	@Column(name = "type")
	private DocumentTypeEnum type;
	
	public String getDocInName() {
		return this.docIn != null ? this.docIn.getPreview() : "";
	}
	
	public String getDocOutName() {
		return this.docOut != null ? this.docOut.getPreview() : "";
	}
	
	public String getDocInNumberOrSign() {
		return this.docIn != null ? this.docIn.getNumberOrSign() : "";
	}
	
	public String getDocOutNumberOrSign() {
		return this.docOut != null ? this.docOut.getNumberOrSign() : "";
	}
		
	public DocumentMeeting(Meeting meeting, Long objId, DocumentTypeEnum type) {
		super();
		this.meeting = meeting;
		this.type = type;
		if (objId == null)
			throw new RestExceptionHandler(Message.INVALID_INPUT_DATA);
		if (DocumentTypeEnum.VAN_BAN_DEN.equals(type)) {
			this.docInId = objId;
		} else {
			this.docOutId = objId;
		}
	}

}
