package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "OUTSIDE_RECEIVE_DOCUMENT", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "doc_id", "address" }) })
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
public class OutsideReceiveDocument extends BaseModel {
	@Column(name = "address")
	private String address;
	@Column(name = "doc_id")
	private Long docId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_id", insertable = false, updatable = false)
	private DocumentOut docOut;
}
