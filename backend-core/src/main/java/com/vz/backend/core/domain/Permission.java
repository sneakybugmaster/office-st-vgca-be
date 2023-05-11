package com.vz.backend.core.domain;

import java.io.Serializable;

import javax.persistence.*;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * @author DucND
 * @date May 07, 2020
 */
@Entity
@Table(name = "PERMISSION", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "sys_module_id", "sys_role_id" }) },
		indexes = {@Index(name = "INDEX_PERMISSION",columnList = "sys_module_id,sys_role_id")})
@Getter
@Setter
@NoArgsConstructor
public class Permission extends BaseModel implements Serializable {
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "id")
	@SequenceGenerator(name = "vz.permission_id_seq", sequenceName = "vz.permission_id_seq", allocationSize = 1)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "vz.permission_id_seq")
	private Long id;

	@Column(name = "sys_module_id")
	private Long moduleId;
	@Column(name = "sys_role_id")
	private Long roleId;

	@OneToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "sys_module_id", insertable = false, updatable = false)
	private Module module;
	@OneToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "sys_role_id", insertable = false, updatable = false)
	private Role role;
	
	public Permission(Long moduleId, Long roleId, Long clientId, Long createBy) {
		super();
		this.moduleId = moduleId;
		this.roleId = roleId;
		this.setCreateBy(createBy);
		this.setClientId(clientId);
	}
	
}
