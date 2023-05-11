package com.vz.backend.business.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class FieldDto {
	private String id;
	private String name;
	private String type;
	private FieldOptionDto[] fieldOption;
	private String catId;
	private String label;
	private boolean required;
	private String placeholder;
}
