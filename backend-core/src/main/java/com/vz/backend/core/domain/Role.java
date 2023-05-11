package com.vz.backend.core.domain;

import java.util.Collection;

import javax.persistence.*;

import org.hibernate.annotations.WhereJoinTable;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * @author DucND
 * @date May 07, 2020
 */
@Entity
@Table(name = "SYS_ROLE", schema = "vz")
@Getter
@Setter
@JsonIgnoreProperties(value = {"handler", "hibernateLazyInitializer"})
@AllArgsConstructor
@NoArgsConstructor
public class Role extends BaseModel {
	private static final long serialVersionUID = 1L;

	@Column(name = "[name]")
	private String name;

	@Column(name = "is_default")
	private Boolean isDefault;

	@ManyToMany(cascade = CascadeType.MERGE, fetch = FetchType.EAGER)
	@JoinTable(name = "PERMISSION", schema = "vz", joinColumns = {
			@JoinColumn(name = "sys_role_id") }, inverseJoinColumns = { @JoinColumn(name = "sys_module_id") })
	@WhereJoinTable(clause = "active=true")
	private Collection<Module> modules;

	private Boolean cabinet;

	@Override
	public boolean equals(Object anObject) {
		if (!(anObject instanceof Role)) {
			return false;
		}
		Role otherObject = (Role) anObject;
		return otherObject.getId().equals(this.getId());
	}

	public Role(String name, Long clientId, Long createBy, boolean cabinet) {
		super();
		this.name = name;
		this.setActive(true);
		this.setCreateBy(createBy);
		this.setClientId(clientId);
		if(Boolean.TRUE.equals(cabinet)) {
			this.cabinet = cabinet;
		}
	}

}
