package com.vz.backend.core.domain;

import java.util.Date;

import javax.persistence.*;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "SYS_STRACE", schema = "vz",indexes = {@Index(name = "INDEX_STRACE",columnList = "id,id_content")})
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class StraceSystem {

	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private Long id;

	@Column(name = "username", nullable = false)
	private String userName;

	@Column(name = "ip_device", length = 50)
	private String ipDevice;

	@Column(name = "name_device", length = 50)
	private String nameDevice;

	@Column(name = "action")
	private String action;

	@JoinColumn(name = "id_cat")
	private Long idCat;

	@Column(name = "id_client")
	private Long clientId;

	@Column(name = "id_content")
	private Long contentId;

	@Column(columnDefinition = "TEXT", name = "content")
	private String content;

	@Column(name = "create_date")
	private Date createDate;
}
