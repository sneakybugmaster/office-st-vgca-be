package com.vz.backend.business.domain;

import java.util.Date;

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
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "OBJECT_TAG", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "obj_id", "tag_id", "type", "client_id" }) })
@Data
@AllArgsConstructor
@NoArgsConstructor
public class ObjectTag extends BaseModel {

	@Column(name = "obj_id")
	private Long objId;

	@Column(name = "tag_id")
	private Long tagId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "tag_id", updatable = false, insertable = false)
	private Tag tag;

	@Column(name = "type")
	@Enumerated(EnumType.STRING)
	private DocumentTypeEnum type;
	
	@Column(name = "preview_name")
	private String previewName;
	
	@Column(name = "issue_date")
	private Date issueDate;

	public ObjectTag(Long objId, Long tagId, DocumentTypeEnum type, String previewName) {
		super();
		this.objId = objId;
		this.tagId = tagId;
		this.type = type;
		this.previewName = previewName;
	}
	
}
