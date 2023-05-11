package com.vz.backend.core.domain;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.OneToMany;
import javax.persistence.OrderBy;
import javax.persistence.Table;

import org.hibernate.annotations.Where;

import com.vz.backend.core.common.BussinessCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "FORMULA", schema = "vz")
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class Formula extends BaseModel {

	@Column(name = "[name]", columnDefinition = "TEXT")
	private String name;

	@Column(name = "type", columnDefinition = "TEXT")
	private String type;

	@Column(name = "description", columnDefinition = "TEXT")
	private String description;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "formula", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active = 'TRUE'")
	@OrderBy(value = "classification")
	private List<FormulaDetail> details = new ArrayList<>();

	@Override
	public void valids() {
		BussinessCommon.require("Tên công thức", this.name);
		BussinessCommon.require("Loại công thức", this.type);
		BussinessCommon.validLengthData(this.name, "Tên công thức", 500);
		BussinessCommon.validLengthData(this.type, "Loại công thức", 500);
	}

	public String calculator(float value) {
		for (FormulaDetail i : this.details) {
			if ((i.getFrom() == null || Float.compare(value, i.getFrom().longValue()) >= 0)
					&& (i.getTo() == null || Float.compare(value, i.getTo().longValue()) <= 0))
				return i.getClassification();
		}
		return "";
	}

	public Formula set (Formula f) {
		this.description = f.getDescription();
		this.name = f.getName();
		this.type = f.getType();
		return this;
	}
}
