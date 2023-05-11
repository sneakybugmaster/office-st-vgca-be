package com.vz.backend.business.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.vz.backend.core.config.DocumentTypeEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ObjectTagDto {
	private Long id;
	private Long objId;
	private Long tagId;
	private DocumentTypeEnum type;
	private String tagName;
	private String objName;
	private String typeName;
	private String code;
	@JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd/MM/yyyy")
	private Date date;

	public ObjectTagDto(Long id, Long objId, Long tagId, DocumentTypeEnum type, String tagName) {
		super();
		this.id = id;
		this.objId = objId;
		this.tagId = tagId;
		this.type = type;
		this.tagName = tagName;
	}

	public ObjectTagDto(Long id, Long objId, Long tagId, DocumentTypeEnum type, String tagName, String objName, Date issueDate) {
		super();
		this.id = id;
		this.objId = objId;
		this.tagId = tagId;
		this.type = type;
		this.tagName = tagName;
		this.objName = objName;
		this.typeName = type.getName();
		this.date = issueDate;
	}

}
