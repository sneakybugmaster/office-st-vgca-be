package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.vz.backend.core.domain.BaseModel;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "CLERICAL_ORG", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "user_id", "org_id" }) })
@Getter
@Setter
@NoArgsConstructor
public class ClericalOrg extends BaseModel{
	private static final long serialVersionUID = 1L;
	@Column(name = "user_id")
	private Long userId;
	@Column(name = "org_id")
	private Long orgId;
}
