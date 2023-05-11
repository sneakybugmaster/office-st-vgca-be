package com.vz.backend.core.dto;

import com.vz.backend.core.common.BussinessCommon;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class FormulaDto extends SearchDto {

	private Long id;
	private String name;
	private String type;

	public String getName() {
		return BussinessCommon.convert(this.name);
	}

	public String getType() {
		return BussinessCommon.convert(this.type);
	}

	public FormulaDto(Long id, String name) {
		super();
		this.id = id;
		this.name = name;
	}
}
