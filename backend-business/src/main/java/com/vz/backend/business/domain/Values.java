package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "[VALUES]", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Values extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "cat_id", nullable = false)
	private Long catId;

	@Column(name = "form_id", nullable = false)
	private Long formId;

	@Column(name = "content")
	private String content;

	@ManyToOne
	@JoinColumn(name = "field_id", nullable = false)
	private Fields fields;
}
