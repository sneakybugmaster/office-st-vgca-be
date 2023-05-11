package com.vz.backend.business.dto;

import java.io.Serializable;

import com.vz.backend.core.domain.Category;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class CategoryDto implements Serializable {
	private static final long serialVersionUID = 1L;

	Long id;
	String name;
	Long categoryTypeId;

	public CategoryDto(Category category) {
		if (category == null) {
			return;
		}
		this.id = category.getId();
		this.name = category.getName();
		this.categoryTypeId = category.getCategoryTypeId();
	}

	public CategoryDto(Long id, String name) {
		this.id = id;
		this.name = name;
	}
}
