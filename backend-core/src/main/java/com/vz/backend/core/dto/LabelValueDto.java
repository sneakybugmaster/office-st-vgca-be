package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class LabelValueDto<T> {
	private T value;
	private T label;
	private T tmp;
	
	public static LabelValueDto<String> set(Object value, String label) {
		return new LabelValueDto<>(value != null ? value.toString() : "", label);
	}
	
	public LabelValueDto(T value, T label) {
		super();
		this.value = value;
		this.label = label;
	}
}
