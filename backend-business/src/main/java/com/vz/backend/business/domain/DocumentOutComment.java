package com.vz.backend.business.domain;

import java.util.List;

import javax.persistence.*;

import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "DOCUMENT_OUT_COMMENT", schema = "vz", indexes = {@Index(name = "INDEX_DOCUMENT_OUT_COMMENT",columnList = "id,doc_id,user_id")})
@Data
@AllArgsConstructor
@NoArgsConstructor
public class DocumentOutComment extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "doc_id")
	private Long docId;

	@Column(columnDefinition = "TEXT", name = "comment")
	private String comment;
	
	@Column(name = "isToken")
	private Boolean isToken;

	@Column(name = "user_position")
	private String userPosition;

	@Column(name = "user_id")
	private Long userId;
	
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "user_id", updatable = false, insertable = false)
	private User user;

	@Transient
	private String userFullName;

	@OneToMany(fetch = FetchType.EAGER, mappedBy = "cmtId")
	private List<DocumentOutAttachment> attachments;
}
