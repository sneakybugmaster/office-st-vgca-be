package com.vz.backend.business.domain;

import javax.persistence.*;

import com.vz.backend.core.domain.BaseModel;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "DOCUMENT_IN_OUT", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "doc_in_id", "doc_out_id" }) },
		indexes = {@Index(name = "DOC_IN_OUT_INX_ABF",columnList = "id,doc_in_id,doc_out_id")})
@Getter
@Setter
@NoArgsConstructor
public class DocumentInOut extends BaseModel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "doc_in_id")
	private Long docInId;
	@ManyToOne
	@JoinColumn(name = "doc_in_id", insertable = false, updatable = false)
	private Documents docIn;

	@Column(name = "doc_out_id")
	private Long docOutId;
	@ManyToOne
	@JoinColumn(name = "doc_out_id", insertable = false, updatable = false)
	private DocumentOut docOut;

	public DocumentInOut(Long docInId, Long docOutId) {
		super();
		this.docInId = docInId;
		this.docOutId = docOutId;
	}

}
