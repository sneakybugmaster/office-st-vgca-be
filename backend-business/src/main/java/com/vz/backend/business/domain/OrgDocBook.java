package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.vz.backend.core.domain.BaseModel;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "ORG_DOC_BOOK", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "book_id", "org_id" }) })
@Getter
@Setter
@NoArgsConstructor
public class OrgDocBook extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "book_id")
	private Long bookId;
	
	@Column(name = "org_id")
	private Long orgId;

	public OrgDocBook(Long bookId, Long orgId) {
		super();
		this.bookId = bookId;
		this.orgId = orgId;
	}
}
