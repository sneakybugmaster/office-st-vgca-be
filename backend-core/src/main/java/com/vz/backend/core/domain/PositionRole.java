package com.vz.backend.core.domain;

import javax.persistence.*;

import lombok.*;

/**
 * @author DucND
 * @date May 07, 2020
 */
@Entity
@Table(name = "SYS_POSITION_ROLE", schema = "vz",indexes = {@Index(name = "INDEX_POSITION_ROLE",columnList = "sys_position_id,sys_role_id")})
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class PositionRole extends BaseModel {
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "id")
	@SequenceGenerator(name = "vz.sys_position_role_id_seq", sequenceName = "vz.sys_position_role_id_seq")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "vz.sys_position_role_id_seq")
	private Long id;

	@Column(name = "sys_position_id")
	private Long posId;

	@Column(name = "sys_role_id")
	private Long roleId;

	@OneToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "sys_position_id", insertable = false, updatable = false)
	private Category position;

	@OneToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "sys_role_id", insertable = false, updatable = false)
	private Role role;
}
