package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class UserSynchResponse {

	private String mail;

	private String zimbraAccountStatus;

	private String uid;

	private OrganizationDto[] OrganizationIds;

	private String displayName;

	private String chucVuId;

	private Long userId;

	private String mobile;
}
