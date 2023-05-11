package com.vz.backend.business.domain;

import javax.persistence.*;

import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "DOCUMENT_USER", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "doc_id", "user_id", "doc_type" }) },
		indexes = {@Index(name = "DOC_USER_INX_USX",columnList = "id,doc_type,doc_id,user_id")})
@Data
@NoArgsConstructor
@AllArgsConstructor
public class DocumentUser extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Enumerated(EnumType.STRING)
	@Column(name = "doc_type")
	DocumentTypeEnum docType;
	@Column(name = "doc_id")
	Long docId;
	@Column(name = "user_id")
	Long userId;
	@Column(name = "important")
	Boolean important;
}
