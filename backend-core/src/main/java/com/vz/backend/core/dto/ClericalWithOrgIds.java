package com.vz.backend.core.dto;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class ClericalWithOrgIds {
	private UserInfoDto userInfo;
	private List<Long> orgIds;
	
	public ClericalWithOrgIds(Long id, String fullName, String positionName, String orgName) {
		super();
		this.userInfo = new UserInfoDto(id, fullName, positionName, orgName);
	}
}
