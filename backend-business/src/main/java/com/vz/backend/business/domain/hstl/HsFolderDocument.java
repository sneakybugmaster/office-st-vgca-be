package com.vz.backend.business.domain.hstl;

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
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.domain.standard.BaseDoc;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.User;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "HS_FOLDER_DOCUMENT", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "folder_id", "doc_id", "doc_type" }) })
@Getter
@Setter
@NoArgsConstructor
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active" })
public class HsFolderDocument extends BaseDoc {

	private static final long serialVersionUID = 1L;
	@Column(name = "folder_id")
	private Long folderId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "folder_id", updatable = false, insertable = false)
	private HsFolder folder;
	
	@Column(name = "doc_id")
	private Long docId;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "doc_type")
	private DocumentTypeEnum type;
	
	@Column(name = "comment")
	private String comment;
	
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "create_by", updatable = false, insertable = false)
	private User createByObj;
	
	public HsFolderDocument(Documents doc, Long folderId, String comment) {
		super(doc);
		this.folderId = folderId;
		this.docId = doc.getId();
		this.comment = comment;
		this.type = DocumentTypeEnum.VAN_BAN_DEN;
	}
	
	public HsFolderDocument(DocumentOut doc, Long folderId, String comment) {
		super(doc);
		this.folderId = folderId;
		this.docId = doc.getId();
		this.comment = comment;
		this.type = DocumentTypeEnum.VAN_BAN_DI;
	}
}
