package com.vz.backend.business.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
@AllArgsConstructor
public class BpmnActiveWrapper {
	private Boolean active;
	private String name;
}
