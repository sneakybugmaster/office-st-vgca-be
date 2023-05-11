package com.vz.backend.core.domain;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;

import lombok.*;

@Entity
@Table(name = "OUTSIDE_SYSTEM", schema = "vz")
@JsonIgnoreProperties({ "handler", "hibernateLazyInitializer", "createDate", "updateDate", "createBy", "updateBy" })
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class OutsideSystem extends BaseModel {
	@Column(nullable = false, unique = true)
	private String name;
	@Column(nullable = false, unique = true)
	private String domain;
	@Column(name = "[key]", nullable = false, unique = true)
	private String key;
	@Column(columnDefinition = "TEXT")
	@JsonIgnore
	private String token;
	private Date timeExpired;
	@Transient
	private String frDomain;

	@Override
	public void valids() {
		BussinessCommon.require("URL", this.domain);
		BussinessCommon.require("Khóa", this.key);
		BussinessCommon.validLengthData(this.domain, "URL", 50);
		BussinessCommon.validLengthData(this.key, "Khóa", 50);
//		BussinessCommon.require("Token", this.token);
		BussinessCommon.require("Tên hệ thống", this.name);
		BussinessCommon.validLengthData(this.name, "Tên hệ thống", 50);
	}

	public void set(OutsideSystem data) {
		this.domain = data.getDomain();
		this.key = data.getKey();
//		this.token = data.getToken();
		this.name = data.getName();
	}
}
