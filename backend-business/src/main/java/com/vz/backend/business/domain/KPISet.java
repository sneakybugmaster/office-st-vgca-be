package com.vz.backend.business.domain;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.hibernate.annotations.Where;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Formula;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "KPI_SET", schema = "vz")
@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties({"active", "clientId", "createDate", "updateDate", "createBy", "updateBy",
"hibernateLazyInitializer" })
public class KPISet extends BaseModel {

	@Column(name = "[name]", unique = true)
	private String name;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "kpiSets", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active = 'TRUE'")
	private List<Targets> kpis;

	@Column(name = "formula", nullable = false)
	private Long formula;
	@JsonIgnoreProperties("hibernateLazyInitializer")
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "formula", updatable = false, insertable = false)
	private Formula formulas;

	@Override
	public void valids() {
		BussinessCommon.require("Tên bộ KPI", this.name);
		BussinessCommon.require("Công thức áp dụng", this.formula);
		int total = 0;
		for (Targets t : getKpis()) {
			total = total + t.getWeightPercent();
		}

		if (total > 100)
			throw new RestExceptionHandler(Message.WEIGHT_INVALID);
	}

	public List<Targets> getKpis() {
		return this.kpis == null ? new ArrayList<>() : this.kpis;
	}

	public String getKpiFormulaName() {
		return this.formulas != null ? this.formulas.getName() : "";
	}

	public KPISet set (KPISet k) {
		this.name = k.getName();
		this.formula = k.getFormula();
		return this;
	}
}
