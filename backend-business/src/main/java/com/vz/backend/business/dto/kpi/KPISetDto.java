package com.vz.backend.business.dto.kpi;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.dto.SearchDto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@AllArgsConstructor
@NoArgsConstructor
public class KPISetDto extends SearchDto {
	private Long id;
	private String name;
	private Long formula;
	
	public String getName() {
		return BussinessCommon.convert(this.name);
	}
	
	public KPISetDto(Long id, String name) {
		this.id = id;
		this.name = name;
	}
	
}
