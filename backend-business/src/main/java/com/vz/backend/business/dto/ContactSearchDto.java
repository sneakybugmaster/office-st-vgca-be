package com.vz.backend.business.dto;

import com.vz.backend.core.dto.SearchDto;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public class ContactSearchDto extends SearchDto {
	public String q;
	public Long orgId;
}
