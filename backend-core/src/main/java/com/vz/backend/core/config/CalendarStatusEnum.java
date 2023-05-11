package com.vz.backend.core.config;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.core.dto.LabelValueDto;

public enum CalendarStatusEnum {
	CANCEL("Hủy duyệt"), PRE_APPROVE("Chờ duyệt"), APPROVE("Đã duyệt"), RETURN("Từ chối");

	private final String name;

	CalendarStatusEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public static CalendarStatusEnum getEnum(String name) {
		for (CalendarStatusEnum v : values()) {
			if (v.name().equals(name)) {
				return v;
			}
		}
		return null;
	}
	
	public static List<LabelValueDto<String>> get() {
		List<LabelValueDto<String>> data = new ArrayList<>();
		for (CalendarStatusEnum v : values()) {
			data.add(new LabelValueDto<>(v.toString(), v.name));
		}
		return data;
	}
}
