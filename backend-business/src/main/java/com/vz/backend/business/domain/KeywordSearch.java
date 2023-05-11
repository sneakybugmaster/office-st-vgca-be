package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "KEY_SEARCH", schema = "vz")
@Getter
@Setter
@NoArgsConstructor
public class KeywordSearch extends BaseModel {
	@Column(name = "[key]")
	private String key;
	private Long userId;

	public KeywordSearch(String key) {
		super();
		this.key = key;
		this.userId = BussinessCommon.getUserId();
	}
}
