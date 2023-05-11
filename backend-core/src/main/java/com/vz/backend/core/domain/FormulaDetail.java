package com.vz.backend.core.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.vz.backend.core.common.BussinessCommon;

import lombok.*;

@Table(name = "DETAIL_FORMULA", schema = "vz")
@Entity
@NoArgsConstructor
@AllArgsConstructor
@Getter
@Setter
public class FormulaDetail extends BaseModel {

	@Column(name = "fr_value")
	private Long from;
	
	@Column(name = "to_value")
	private Long to;
	
	@Column(name = "classification")
	private String classification;
	
	@Column(name = "color")
	private String color;
	
	@JsonProperty(value = "fId")
	@Column(name = "f_id")
	private Long fId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "f_id", insertable = false, updatable = false)
	private Formula formula;
	
	@Override
	public void valids() {
		BussinessCommon.require("Công thức tính", this.fId);
	}
}
