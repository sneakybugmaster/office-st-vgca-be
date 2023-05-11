package com.vz.backend.core.domain;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "SYS_SECRETARY", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "user_id", "boss_id", "client_id" }) },
		indexes = {@Index(name = "INDEX_SECRETARY",columnList = "boss_id,user_id")})
@Getter
@Setter
@JsonIgnoreProperties({ "handler", "hibernateLazyInitializer", "createDate", "updateDate", "createBy", "updateBy",
		"active", "clientId" })
@NoArgsConstructor
public class Secretary extends BaseModel {
	private static Secretary instance;

	public static Secretary getInstance() {
		if (instance == null) {
			instance = new Secretary();
		}
		return instance;
	}

	@Column(name = "boss_id", nullable = false)
	private Long bossId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "boss_id", insertable = false, updatable = false)
	private User boss;

	@Column(name = "user_id", nullable = false)
	private Long userId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", insertable = false, updatable = false)
	private User user;

	public List<Secretary> add(Long bossId, List<Long> userIds) {
		List<Secretary> rs = new ArrayList<>();
		if (BussinessCommon.isEmptyList(userIds))
			return rs;
		userIds.forEach(i -> rs.add(new Secretary(bossId, i)));
		return rs;
	}

	public Secretary(Long bossId, Long userId) {
		this.userId = userId;
		this.bossId = bossId;
	}
}
