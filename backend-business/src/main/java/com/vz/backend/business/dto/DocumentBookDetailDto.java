package com.vz.backend.business.dto;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class DocumentBookDetailDto {
	private Long id;
	private Boolean active;
	private String name;
	private String numberOrSign;
	private Long bookType;
	private Integer year;
	private Long orgId;
	private String orgName;
	private Long startNumber;
	private Long currentNumber;
	private Long totalDocs;
	private List<Long> orgIds;
	private List<Long> categoryIds;
	
	public DocumentBookDetailDto(Long id, Boolean active, String name, String numberOrSign, Long startNumber, Long currentNumber, Long bookType, Integer year, 
			Long orgId, String orgName) {
		super();
		this.id = id;
		this.active = active;
		this.name = name;
		this.numberOrSign = numberOrSign;
		this.startNumber = startNumber;
		this.currentNumber = currentNumber;
		this.bookType = bookType;
		this.year = year;
		this.orgId = orgId;
		this.orgName = orgName;
	}
	
}
