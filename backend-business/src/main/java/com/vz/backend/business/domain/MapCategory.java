package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Table;

import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "MAP_CATEGORY", schema = "vz", indexes = {@Index(name = "INDEX_MAP_CATEGORY",columnList = "cat_id")})
@Data
@AllArgsConstructor
@NoArgsConstructor
public class MapCategory extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "[name]")
	private String name;

	@Column(name = "cat_id", unique = true)
	private Long catId;
}
