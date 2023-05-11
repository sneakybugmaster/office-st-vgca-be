package com.vz.backend.core.domain;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.config.AuthorityEnum;

import lombok.*;

@Entity
@Table(name = "SYS_AUTHORITY_USER", schema = "vz", indexes = {@Index(name = "INDEX_AUTHORITY_USER",columnList = "user_id,position_id")})

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class AuthorityUser extends BaseModel {
	@Column(name = "user_id", nullable = true)
	private Long userId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", insertable = false, updatable = false, nullable = true)
	private User user;
	
	@Column(name = "authority")
	@Enumerated(EnumType.STRING)
	private AuthorityEnum authority;
	
	@Column(name = "position_id", nullable = true)
	private Long positionId;
}
