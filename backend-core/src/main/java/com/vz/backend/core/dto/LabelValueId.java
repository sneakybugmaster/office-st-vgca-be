package com.vz.backend.core.dto;

import lombok.Data;

@Data
public class LabelValueId<T> extends LabelValueDto<T> {
	private Long id;

	public LabelValueId(Long id, T value, T label) {
		super(value, label);
		this.id = id;
	}
	
	public LabelValueId(Long id, T value, T label, T tmp) {
		super(value, label, tmp);
		this.id = id;
	}
}
