package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class ContactDto {
	private Long id;
	private String fullName;
	private String orgName;
	private String phone;
	private String email;
	private Long orgId;
	private Long parentOrgId;
}
