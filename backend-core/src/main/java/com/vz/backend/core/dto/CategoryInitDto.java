package com.vz.backend.core.dto;

import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
public class CategoryInitDto {
	private String name;
	private Long id;
	private Long value;
	private String numberOrSign;
	private Integer year;
	private Boolean isDefault;

	public CategoryInitDto(String name, Long id) {
		this.name = name;
		this.id = id;
	}

	public CategoryInitDto(Long id, String name, Long value) {
		this.name = name;
		this.id = id;
		this.value = value;
	}

	public CategoryInitDto(Long id, String name, Long value, String numberOrSign) {
		this.name = name;
		this.id = id;
		this.value = value;
		this.numberOrSign = numberOrSign;
	}

	public CategoryInitDto(Long id, String name, Long value, String numberOrSign, Integer year) {
		this.name = name;
		this.id = id;
		this.value = value;
		this.numberOrSign = numberOrSign;
		this.year = year;
	}

	public CategoryInitDto(String name, Long id, Long value, String numberOrSign, Integer year) {
		this.name = name;
		this.id = id;
		this.value = value;
		this.numberOrSign = numberOrSign;
		this.year = year;
	}
}
