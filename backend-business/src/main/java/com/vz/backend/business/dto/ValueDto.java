package com.vz.backend.business.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ValueDto {
	private String id;

	private String catId;

	private String formId;

	private String content;

	private String fieldsId;
}
