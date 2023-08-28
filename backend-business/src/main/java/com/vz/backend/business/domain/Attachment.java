package com.vz.backend.business.domain;

import javax.persistence.*;

import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.config.AttachmentTypeEnum;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "ATTACHMENT", schema = "vz",indexes = {@Index(name = "ATTACH_INX_ATM",columnList = "id,document_id,atm_type")})
@Data
@AllArgsConstructor
@NoArgsConstructor
@Builder
public class Attachment extends AttachmentBase {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "document_id")
	private Long documentId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "document_id", updatable = false, insertable = false)
	private Documents document;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "atm_type")
	private AttachmentTypeEnum atmType;
	
	public Attachment(MultipartFile file) {
		super(file);
	}
	
	public Attachment(MultipartFile file, Long objId, String name) {
		super(file);
		this.documentId = objId;
		super.setName(name);
		super.setEncrypt(true);
	}

	public Attachment(String name, String type, Long size, Long documentId) {
		super(name, type, size);
		this.documentId = documentId;
	}
	public Attachment clone() throws CloneNotSupportedException {
		return (Attachment) super.clone();
	}
}