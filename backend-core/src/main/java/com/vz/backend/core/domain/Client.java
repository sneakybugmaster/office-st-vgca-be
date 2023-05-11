package com.vz.backend.core.domain;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import lombok.Getter;
import lombok.Setter;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Entity
@Table(name = "SYS_CLIENTS", schema = "vz")
@Getter
@Setter
public class Client extends RootModel implements Serializable {
	private static final long serialVersionUID = 1L;

	private String name;
	private String code;
	private String address;
	private String phone;
	private String salt;

	@Id
	@Column(name = "id")
	@SequenceGenerator(name = "vz.sys_clients_id_seq", sequenceName = "vz.sys_clients_id_seq", allocationSize = 1)
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "vz.sys_clients_id_seq")
	private Long id;

	/*
	 * @ManyToOne(fetch = FetchType.EAGER)
	 *
	 * @JoinColumn(name = "customer_type_id") private Category customerType ;
	 *
	 * private String business_type;
	 *
	 * @ManyToOne(fetch = FetchType.EAGER)
	 *
	 * @JoinColumn(name = "customer_use_type_id") private Category categoryUseType;
	 */

	// @ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "customer_type_id")
	private Long customerType;

	private String token;
}
