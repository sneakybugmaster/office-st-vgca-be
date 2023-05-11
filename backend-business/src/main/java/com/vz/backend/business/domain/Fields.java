package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;

import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "FIELDS", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Fields extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "[name]")
	private String name;

	@Column(name = "type")
	private String type;

	@Column(name = "fields_option")
	private String fieldOption;

	@Column(name = "cat_id")
	private Long catId;

	@Column(name = "label")
	private String label;

	@Column(name = "required")
	private boolean required;

	@Column(name = "placeholder")
	private String placeholder;
}
