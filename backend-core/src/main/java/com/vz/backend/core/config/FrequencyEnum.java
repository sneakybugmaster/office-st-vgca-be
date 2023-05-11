package com.vz.backend.core.config;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.core.dto.LabelValueDto;

public enum FrequencyEnum {
	MONTH("Tháng"), 
	YEAR("Năm"), 
	QUARTER("Qúy"), 
	SIX_MONTHS_FIRST_YEAR("6 tháng đầu năm"), 
	SIX_MONTHS_LAST_YEAR("6 tháng cuối năm"),
//	NULL("NULL"),
	;

	private String name;

	public String getName() {
		return this.name;
	}

	FrequencyEnum(String name) {
		this.name = name;
	}
	
	public static List<LabelValueDto<String>> get() {
		List<LabelValueDto<String>> data = new ArrayList<>();
		for (FrequencyEnum v : values()) {
			data.add(new LabelValueDto<>(v.toString(), v.name));
		}
		return data;
	}
}
