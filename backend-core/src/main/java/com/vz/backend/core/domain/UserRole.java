package com.vz.backend.core.domain;

import javax.persistence.*;

import lombok.*;

/**
 * @author DucND
 * @date May 07, 2020
 */
@Entity
@Table(name = "SYS_USER_ROLE", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "sys_user_id", "sys_role_id" }) },
		indexes = {@Index(name = "INDEX_USER_ROLE",columnList = "sys_user_id,sys_role_id")})
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class UserRole extends BaseModel {
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "id")
	@SequenceGenerator(name = "vz.sys_user_role_id_seq", sequenceName = "vz.sys_user_role_id_seq")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "vz.sys_user_role_id_seq")
	private Long id;

	@Column(name = "sys_user_id")
	private Long userId;

	@Column(name = "sys_role_id")
	private Long roleId;

	@OneToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "sys_user_id", insertable = false, updatable = false)
	private User user;

	@OneToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "sys_role_id", insertable = false, updatable = false)
	private Role role;

	public UserRole(Long userId, Long roleId, Long clientId, Long createBy) {
		super();
		this.userId = userId;
		this.roleId = roleId;
		this.setCreateBy(createBy);
		this.setClientId(clientId);
	}
	
}
