package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "OBJECT_READ", schema = "vz")

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ObjectRead extends BaseModel {

	@Column(name = "user_id")
	private Long userId;

	@Enumerated(EnumType.STRING)
	@Column(name = "type")
	private DocumentTypeEnum type;

	@Column(name = "obj_id")
	private Long objId;
	
	@Column(name = "[read]")
	private boolean read = false;
}
