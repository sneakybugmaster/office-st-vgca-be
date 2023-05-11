package com.vz.backend.core.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.StringUtils;

import lombok.Getter;
import lombok.Setter;

@Entity
@Table(name = "SYS_ORG_SIGN", schema = "vz")
@Getter
@Setter
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
public class OrgConfigSign extends BaseModel {
	private static final long serialVersionUID = 1L;
	@Column(unique = true, nullable = false)
	private Long orgId;
	@Column(nullable = false)
	private String place;
	@Column(nullable = false)
	private String tl;
	@Column(nullable = false, name = "user_id")
	private Long userId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", insertable = false, updatable = false)
	private User user;

	public void from(OrgConfigSign config) {
		this.place = config.place;
		this.tl = config.tl;
		this.userId = config.userId;
	}

	public static OrgConfigSign valid(OrgConfigSign config) {
		if (config == null) {
			return config;
		}
		int validField = 0;
		if (!StringUtils.isNullOrEmpty(config.place)) {
			++validField;
		}
		if (config.userId != null && config.userId > 0) {
			++validField;
		}
		if (config.orgId != null && config.orgId > 0) {
			++validField;
		}
		if (!StringUtils.isNullOrEmpty(config.place)) {
			++validField;
		}
		if (validField == 3 || validField == 2) {
			return config;
		}
		if (validField == 0) {
			return null;
		}
		throw new RestExceptionHandler("Vui lòng nhập đầy đủ thông tin cấu hình ký");
	}
}
