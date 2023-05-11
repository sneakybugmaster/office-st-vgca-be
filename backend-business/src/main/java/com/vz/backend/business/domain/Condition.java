package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Organization;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Entity
@Data
@AllArgsConstructor
@NoArgsConstructor
@Table(name = "CONDITION", schema = "vz")
@JsonIgnoreProperties({ "id", "clientId", "createDate", "updateDate", "createBy", "updateBy" })
public class Condition extends BaseModel {

	private static final long serialVersionUID = 1L;

	@Column(name = "org_id")
	private Long orgId;
	@ManyToOne
	@JoinColumn(name = "org_id", updatable = false, insertable = false)
	private Organization org;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "node_id")
	@JsonIgnore
	@ToString.Exclude
	private NodeModel2 node;
	@Column(name = "node_id", updatable = false, insertable = false)
	private Long nodeId;

	@Column(name = "position_id")
	private Long positionId;
	@ManyToOne
	@JoinColumn(name = "position_id", updatable = false, insertable = false)
	private Category position;

	@Column(name = "user_id")
	private Long userId;

	@Column(name = "is_allow_config")
	private Boolean allowConfig = true;

	private Boolean forceSameOrg = false;

	private Boolean subHandle = false;

	private Long orgType;

	private Boolean security;
	
	private boolean wrap(Boolean value) {
		return wrap(value, false);
	}
	private boolean wrap(Boolean value, boolean defaultValue) {
		if (value == null) {
			return defaultValue;
		}
		return value;
	}

	public void setSecurity(Boolean security) {
		this.security = wrap(security);
	}

	public Boolean getSecurity() {
		return wrap(this.security);
	}

	public void setAllowConfig(Boolean allowConfig) {
		this.allowConfig = wrap(allowConfig);
	}

	public Boolean getAllowConfig() {
		return wrap(this.allowConfig);
	}

	public void setForceSameOrg(Boolean forceSameOrg) {
		this.forceSameOrg = wrap(forceSameOrg);
	}

	public Boolean getForceSameOrg() {
		return wrap(this.forceSameOrg);
	}

	public void setSubHandle(Boolean subHandle) {
		this.subHandle = wrap(subHandle);
	}

	public Boolean getSubHandle() {
		return wrap(this.subHandle);
	}

	public void clone(Condition condition) {
		this.orgId = condition.orgId;
		this.positionId = condition.positionId;
		this.userId = condition.userId;
		this.allowConfig = condition.getAllowConfig();
		this.forceSameOrg = condition.getForceSameOrg();
		this.subHandle = condition.getSubHandle();
		this.orgType = condition.getOrgType();
		this.security = condition.getSecurity();
	}

	public boolean valid() {
		return this.orgId != null || this.positionId != null || this.userId != null;
	}
}
