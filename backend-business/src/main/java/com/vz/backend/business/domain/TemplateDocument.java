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

import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.config.CalendarStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "TEMPLATE", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TemplateDocument extends AttachmentBase {
	
	@Column(name = "t_name")
	private String tName;
	
	@Column(name = "approve_id")
	private Long approveId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "approve_id", updatable = false, insertable = false)
	private User approver;
	
	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private CalendarStatusEnum status;
	
	@Column(name = "doc_type")
	@Enumerated(EnumType.STRING)
	private DocumentTypeEnum docType;
	
	public TemplateDocument(MultipartFile file, DocumentTypeEnum type, String tName) {
		super(file);
		this.docType = type;
		this.tName = tName;
	}
	
	@PrePersist
	public void prePersit() {
		this.status = CalendarStatusEnum.PRE_APPROVE;
	}
	
	public String getStatusName() {
		return this.status.getName();
	}
}
