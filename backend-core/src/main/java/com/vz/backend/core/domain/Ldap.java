package com.vz.backend.core.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

/**
 * @author DucND
 * @date 21 thg 7, 2020
 */
@Entity
@Table(name = "SYS_LDAP", schema = "vz")
@Getter
@Setter
public class Ldap extends BaseModel {
	private static final long serialVersionUID = 1L;

	@Column(name = "url")
	private String url;

	@Column(name = "authen_type")
	private String authenType;

	@Column(name = "principal")
	private String principal;

	@Column(name = "password")
	private String password;

	@Column(name = "context_factory")
	private String contextFactory;

	@Column(name = "attributes")
	private String attributes;

	@Column(name = "domain")
	private String domain;

	@Column(name = "filter")
	private String filter;

}