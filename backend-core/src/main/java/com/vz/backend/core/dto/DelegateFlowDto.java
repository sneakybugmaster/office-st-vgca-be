package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class DelegateFlowDto {
	private Long id;
	private String fromPosition;
	private String toPosition;
}
