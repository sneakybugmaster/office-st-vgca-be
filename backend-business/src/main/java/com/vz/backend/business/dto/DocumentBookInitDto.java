package com.vz.backend.business.dto;

import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class DocumentBookInitDto {
	private Long id;
	private String name;
	private String numberOrSign;
	private Integer year;
	private List<Long> securityIds;
	private Long value;
	private Long currentNumber;
	private Long totalDocs;
}
