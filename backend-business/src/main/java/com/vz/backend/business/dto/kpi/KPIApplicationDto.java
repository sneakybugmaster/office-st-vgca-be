package com.vz.backend.business.dto.kpi;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.FrequencyEnum;
import com.vz.backend.core.dto.SearchDto;

import lombok.Data;

@Data
public class KPIApplicationDto extends SearchDto {
	private Long id;
	public String name;
	public Integer year;
	public Long kpiSet;
	public FrequencyEnum frequency;
	public String month;

	public String getName() {
		return BussinessCommon.convert(this.name);
	}
	
	public KPIApplicationDto() {
		
	}
	
	public KPIApplicationDto(Long id, String name, Integer year) {
		this.id = id;
		this.name = name;
		this.year = year;
	}
}
