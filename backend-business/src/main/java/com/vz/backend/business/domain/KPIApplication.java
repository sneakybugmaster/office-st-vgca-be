package com.vz.backend.business.domain;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import org.hibernate.annotations.Where;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.dto.kpi.KPIResultDto;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.FrequencyEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Formula;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "KPI_APPLICATION", schema = "vz",  uniqueConstraints = {
		@UniqueConstraint(columnNames = { "name", "client_id" }) })
@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties({"active", "clientId", "createDate", "updateDate", "createBy", "updateBy",
"hibernateLazyInitializer" })
public class KPIApplication extends BaseModel {

	@Column(name = "[name]", nullable = false)
	private String name;

	/**
	 * Năm áp dụng
	 */
	@Column(name = "year", nullable = false)
	private Integer year;

	@Column(name = "kpi_set", nullable = false)
	private Long kpiSet;
	@JsonIgnore
	@JsonIgnoreProperties("hibernateLazyInitializer")
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "kpi_set", insertable = false, updatable = false)
	private KPISet kpiSets;

	/**
	 * Tháng áp dụng (có thể chọn nhiều tháng)
	 */
	@Column(name = "months", nullable = false)
	private String months;

	/**
	 * Tổ chức áp dụng
	 */
	@Column(name = "orgs")
	private String orgs;

	/**
	 * Người áp dụng
	 */
	@Column(name = "users")
	private String users;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "kpiApp", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active = 'TRUE'")
	public List<KPIUser> kpiUsers = new ArrayList<>();

	/**
	 * tần suất đo
	 */
	@Column(name = "frequency", nullable = false)
	@Enumerated(EnumType.STRING)
	private FrequencyEnum frequency;

	@Column(name = "quarter")
	private Integer quarter;

	@Transient
	public List<Long> orgIds = new ArrayList<>();

	@Transient
	public List<Long> userIds = new ArrayList<>();

	@Transient
	private List<Integer> monthIds = new ArrayList<>();

	@JsonIgnore
	@Transient
	private List<Organization> orgLists = new ArrayList<>();

	@JsonIgnore
	@Transient
	private List<User> userLists = new ArrayList<>();

	@Override
	public void valids() {
		BussinessCommon.require("Tên phiếu giao KPI", this.name);
		BussinessCommon.require("Năm đánh giá", this.year);
		BussinessCommon.require("Tần suất đo", this.frequency);
		BussinessCommon.require("Bộ KPI áp dụng", this.kpiSet);

		if(FrequencyEnum.MONTH.equals(this.frequency)) {
			BussinessCommon.require("Tháng áp dụng", this.monthIds);
		}

		if(FrequencyEnum.QUARTER.equals(this.frequency)) {
			BussinessCommon.require("Qúy áp dụng", this.quarter);
			if (this.quarter > 4 || this.quarter < 1)
				throw new RestExceptionHandler("Qúy áp dụng không hợp lệ");
		}

		if (BussinessCommon.isEmptyList(this.orgIds) && BussinessCommon.isEmptyList(this.userIds))
			throw new RestExceptionHandler(Message.KPI_APPLICATION_NO_USER);
	}

	public List<Long> getUserIds() {
		return BussinessCommon.toListLong(this.users);
	}

	public List<Long> getOrgIds() {
		return BussinessCommon.toListLong(this.orgs);
	}

	public List<Integer> getMonthIds() {
		return BussinessCommon.toListInteger(this.months);
	}

	public String getFrequencyName() {
		return this.frequency != null ? this.frequency.getName() : "";
	}

	public void set(KPIApplication k) {
		if(k.getId() != null) super.setId(k.getId());
		this.name = k.getName();
		this.year = k.getYear();
		this.kpiSet = k.getKpiSet();
		this.orgs = BussinessCommon.joinOrStar(k.orgIds, "-");
		this.users = BussinessCommon.joinOrStar(k.userIds, "-");
		this.months = BussinessCommon.joinOrStar(k.monthIds, "-");
		this.frequency = k.getFrequency();
		this.quarter = k.getQuarter();
		this.setActive(true);
	}

	public List<KPIResultDto> getKpiUsers() {
		return calculator(KPIUser.convert(this.kpiUsers));
	}

	public List<KPIResultDto> calculator(List<KPIResultDto> rs) {
		rs.forEach(i -> i.setPoint(getFormula().calculator(i.getTotal())));
		return rs;
	}

	public List<IdName> getOrgObjs() {
		List<IdName> orgObjs = new ArrayList<>();
		this.getOrgLists().forEach(i -> orgObjs.add(new IdName(i.getId(), i.getName())));
		return orgObjs;
	}

	public List<IdName> getUserObjs() {
		List<IdName> userObjs = new ArrayList<>();
		this.getUserLists().forEach(i -> userObjs.add(new IdName(i.getId(), i.getFullName())));
		return userObjs;
	}

	@JsonIgnore
	public Formula getFormula() {
		try {
			return this.getKpiSets().getFormulas();
		} catch (Exception e) {
			e.printStackTrace();
			throw new RestExceptionHandler(Message.KPI_CAL_SYS_ERROR);
		}
	}
}
