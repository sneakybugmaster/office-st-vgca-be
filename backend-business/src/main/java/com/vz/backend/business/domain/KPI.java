package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "KPI", schema = "vz")
@AllArgsConstructor
@NoArgsConstructor
@Data
@JsonIgnoreProperties({"clientId", "createDate", "updateDate", "createBy", "updateBy",
"hibernateLazyInitializer" })
public class KPI extends BaseModel {

	@Column(name ="code")
	private String code;
	
	@Column(name ="name")
	private String name;
	
	/**
	 * Loại đối tượng / viễn cảnh
	 */
	@Column(name = "type")
	@Enumerated(EnumType.STRING)
	private DocumentTypeEnum typeObj;
	
	@Column(name = "user_id")
	private Long userId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", insertable = false, updatable = false)
	private User user;
	
	@Override
	public void valids() {
		BussinessCommon.require("Mã KPI", this.code);
		BussinessCommon.require("Tên KPI", this.name);
		BussinessCommon.require("Loại đối tượng", this.typeObj);
	}
	
	public String getTypeObjName() {
		return this.typeObj != null ?  this.typeObj.getName() : "";
	}
	
	public String createBy() {
		return this.user != null ? this.user.getFullName() : "";
	}
	
	@PrePersist
	@PreUpdate
	public void prePush() {
		this.userId = BussinessCommon.getUserId();
	}
	
	public KPI set(KPI kpi) {
		this.code = kpi.getCode();
		this.name = kpi.getName();
		this.typeObj = kpi.getTypeObj();
		return this;
	}
}
