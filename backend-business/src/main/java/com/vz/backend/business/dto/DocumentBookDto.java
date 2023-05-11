package com.vz.backend.business.dto;

import java.util.List;

import lombok.Data;

@Data
public class DocumentBookDto {

	private Long id;
	private Boolean active;
	private String name;
	private Long startNumber;
	private Long currentNumber;
	private Long bookType;
	private String numberOrSign;
	private Integer year;
	private Long orgId;
	private String orgName;
	private List<Long> orgIds;

	public DocumentBookDto(Long id, Boolean active, String name, Long startNumber, Long currentNumber, Long bookType,
			String numberOrSign, Integer year, Long orgId, String orgName, List<Long> orgIds) {
		this.id = id;
		this.active = active;
		this.name = name;
		this.startNumber = startNumber;
		this.currentNumber = currentNumber;
		this.bookType = bookType;
		this.numberOrSign = numberOrSign;
		this.year = year;
		this.orgId = orgId;
		this.orgName = orgName;
		this.orgIds = orgIds;
	}

	public DocumentBookDto(Long id, Boolean active, String name, Long startNumber, Long currentNumber, Long bookType,
			String numberOrSign, Integer year, Long orgId, String orgName) {
		this(id, active, name, startNumber, currentNumber, bookType, numberOrSign, year, orgId, orgName, null);
	}
}
