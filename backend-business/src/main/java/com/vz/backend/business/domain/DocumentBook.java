package com.vz.backend.business.domain;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Entity
@Table(name = "DOCUMENT_BOOK", schema = "vz", indexes = {@Index(name = "DOCBOOK_INX_DOCBO",columnList = "id,current_number,number_sign,year,org_create_id")})
@Data
@NoArgsConstructor
@JsonIgnoreProperties({ "hibernateLazyInitializer" })
public class DocumentBook extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "[name]")
	private String name;
	@Column(name = "start_number")
	private Long startNumber;
	@Column(name = "current_number")
	private Long currentNumber;
	@Column(name = "book_type")
	private Long bookType;
	@Column(name = "number_sign")
	private String numberOrSign;
	@Column(name = "year")
	private Integer year;
	@Column(name = "org_create_id")
	private Long orgCreateId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "org_create_id", updatable = false, insertable = false)
	private Organization org;

	public DocumentBook(Long id, String name, Long currentNumber, Long bookType, String numberOrSign, Integer year) {
		super.setId(id);
		this.name = name;
		this.currentNumber = currentNumber;
		this.bookType = bookType;
		this.numberOrSign = numberOrSign;
		this.year = year;
		this.orgCreateId = BussinessCommon.getUser().getOrg();
	}

	public DocumentBook(DocumentBook db) {
		super();
		this.name = db.getName();
		this.bookType = db.getBookType();
		this.numberOrSign = db.getNumberOrSign();
		this.year = db.getYear();
	}

	public void setYear(Integer year) {
		if (year == null) {
			throw new RestExceptionHandler(Message.DB_BOOK_YEAR);
		}
		this.year = year;
	}

	@PrePersist
	public void setOrgCreate() {
		this.orgCreateId = BussinessCommon.getUser().getOrg();
		if (this.currentNumber == null) this.currentNumber = 0L;
	}

	public Long getCurrentNumber() {
		return this.currentNumber == null ? 0 : this.currentNumber.longValue();
	}
}
