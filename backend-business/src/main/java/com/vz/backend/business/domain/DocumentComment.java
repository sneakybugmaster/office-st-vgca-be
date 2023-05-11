package com.vz.backend.business.domain;

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.config.DocumentCommentTypeEnum;
import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "DOCUMENT_COMMENT", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DocumentComment extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "user_full_name")
	private String userFullName;

	@Column(name = "user_position")
	private String userPosition;

	private String userOrg;
	
	@Column(columnDefinition = "TEXT", name = "cmt_content")
	private String cmtContent;
	
	@Column(name = "isToken")
	private Boolean isToken;

	@Column(name = "doc_id")
	private Long docId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_id", updatable = false, insertable = false)
	private Documents document;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "commentId")
	private List<AttachmentComment> attachments;
	
	@Column(columnDefinition = "TEXT", name = "comment")
	private String comment;
	
	@Column(name = "isTransfer")
	private Boolean isTransfer;
	
	@Column(name = "type")
	@Enumerated(EnumType.STRING)
	private DocumentCommentTypeEnum type;

	public DocumentComment(Long docId, String comment) {
		super();
		this.comment = comment;
		this.docId = docId;
	}
}
