package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.SourceKPIEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "TARGETS", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "client_id", "kpi", "kpi_set" }) })
@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties({"active", "clientId", "createDate", "updateDate", "createBy", "updateBy",
"hibernateLazyInitializer" })
public class Targets extends BaseModel {

	/**
	 * Phân bổ trọng số
	 * Tổng trong cùng 1 set không quá 100
	 */
	@Column(name = "weight", nullable = false)
	private int weightPercent;
	
	/**
	 * Chiều hướng tăng
	 */
	@Column(name = "up_trend")
	private boolean upTrend;
	
	/**
	 * Tự động tính kết quả
	 */
	@Column(name = "auto_result")
	private boolean autoResult;
	
	@Column(name = "description", columnDefinition = "TEXT")
	private String desciption;
	
	@Column(name = "kpi", nullable = false)
	private Long kpi;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "kpi", insertable = false, updatable = false)
	private KPI kpiObj;
	
	@Column(name = "kpi_set")
	private Long kpiSet;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "kpi_set", insertable = false, updatable = false)
	private KPISet kpiSets;
	
	@Column(name = "source", nullable = false)
	@Enumerated(EnumType.STRING)
	private SourceKPIEnum source;
	
	@Override
	public void valids() {
		BussinessCommon.require("Bộ KPI", this.kpiSet);
		BussinessCommon.require("Phân bổ trọng số", this.weightPercent);
		if (this.weightPercent < 0 || this.weightPercent > 100)
			throw new RestExceptionHandler(Message.WEIGHT_INVALID);
		BussinessCommon.require("Nguồn KPI", this.source);
		BussinessCommon.require("KPI", this.kpi);
	}
	
	public String getName() {
		return this.kpiObj != null ? this.kpiObj.getName() : "";
	}
	
	public String getSrcName() {
		return this.source != null ? this.source.getName() : "";
	}
	
	public String getCode() {
		return this.kpiObj != null ? this.kpiObj.getCode() : "";
	}
	
	public Targets set(Targets t) {
		this.weightPercent = t.getWeightPercent();
		this.upTrend = t.isUpTrend();
		this.autoResult = t.isAutoResult();
		this.desciption = t.getDesciption();
		this.source = t.getSource();
		this.kpiSet = t.getKpiSet();
		this.kpi = t.getKpi();
		this.setActive(true);
		return this;
	}
}
