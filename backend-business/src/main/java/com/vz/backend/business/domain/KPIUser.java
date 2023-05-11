package com.vz.backend.business.domain;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.config.ReceiveTypeEnum;
import com.vz.backend.business.dto.kpi.KPIResultDto;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "KPI_USER", schema = "vz")
@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties({ "clientId", "createDate", "updateDate", "createBy", "updateBy",
"hibernateLazyInitializer" })
public class KPIUser extends BaseModel {

	@Column(name = "user_id")
	private Long userId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", insertable = false, updatable = false)
	private User user;
	
	@Column(name = "target_id")
	private Long targetId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "target_id", insertable = false, updatable = false)
	private Targets targets;
	
	/**
	 * Chỉ tiêu : bao nhiêu thì đạt abc
	 */
	@Column(name = "target", columnDefinition = "int default 1")
	private int target;
	
	@Column(name = "kpi_app_id")
	private Long kpiAppId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "kpi_app_id", insertable = false, updatable = false)
	private KPIApplication kpiApp;
	
	@Column(name = "type")
	@Enumerated(EnumType.STRING)
	private ReceiveTypeEnum type;
	
	/**
	 * Số liệu thực tế
	 */
	@Transient
	private int actual;
	
	/**
	 * Hiệu suất
	 * @return
	 */
	@Transient
	private float performance;
	
	/**
	 * Điểm quy đổi
	 * @return
	 */
	@Transient
	private float exchange;
	
	@Override
	public void valids() {
		BussinessCommon.require("Mã chỉ tiêu", this.targetId);
		BussinessCommon.require("Phiếu giao KPI", this.kpiAppId);
		BussinessCommon.require("Người được giao", this.userId);
		if(target < 1) throw new RestExceptionHandler(Message.INVALID_TARGETS);
	}
	
	public KPIUser(Long userId, Long targetId, Long kpiAppId, ReceiveTypeEnum type) {
		this.userId = userId;
		this.targetId = targetId;
		this.kpiAppId = kpiAppId;
		this.type = type;
	}
	
	public KPIUser(Long userId, Long targetId, Long kpiAppId) {
		this.userId = userId;
		this.targetId = targetId;
		this.kpiAppId = kpiAppId;
	}
	
	public String getKpiName() {
		return this.targets != null && this.targets.getKpiObj() != null ? this.targets.getKpiObj().getName() : "";
	}

	public int getWeightPercent() {
		return this.targets != null ? this.targets.getWeightPercent() : 1;
	}
	
	public float getPerformance() {
		return toPerformance();
	}
	
	public float getExchange() {
		return toExchange();
	}
	
	public static List<KPIResultDto> convert(List<KPIUser> kpis) {
		List<KPIResultDto> results = new ArrayList<>();
		Map<KPIResultDto, List<KPIUser>> map = new HashMap<>();
		kpis.forEach(i -> {
			List<KPIUser> kpiUsers = new ArrayList<>();
			KPIResultDto key = new KPIResultDto(i.getUser());
			if (map.containsKey(key)) {
				kpiUsers = map.get(key);
			}
			kpiUsers.add(i);
			map.put(key, kpiUsers);
		});
		
		map.forEach((k, v) -> {
			k.setKpis(v);
			k.setTotal(v);
			results.add(k);
		});
		
		return results;
	}
	
	public String fullName() {
		return this.user != null ? this.user.getFullName() : "";
	}
	
	public String positionName() {
		return this.user != null ? this.user.getPositionModel().getName() : "";
	}
	
	public String OrgName() {
		return this.user != null ? this.user.getOrgModel().getName() : "";
	}
	
	public float toPerformance() {
		float temp = this.getTarget() == 0 ? 1 : (((float) this.getActual()) / this.getTarget());
		return (float) Math.round(temp * 100);
	}
	
	public float toExchange() {
		int weightPercent = this.getTargets() == null ? 0 : this.getTargets().getWeightPercent();
		float temp = toPerformance() * (((float) weightPercent) / 100);
		return (float) Math.round(temp * 100) / 100;
	}
}
