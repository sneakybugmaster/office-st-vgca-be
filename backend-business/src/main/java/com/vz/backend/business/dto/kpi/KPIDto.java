package com.vz.backend.business.dto.kpi;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.dto.SearchDto;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class KPIDto extends SearchDto {
	private String code;
	private String name;
	public DocumentTypeEnum typeObj;

	public String getName() {
		return BussinessCommon.convert(this.name);
	}

	public String getCode() {
		return BussinessCommon.convert(this.code);
	}
}
