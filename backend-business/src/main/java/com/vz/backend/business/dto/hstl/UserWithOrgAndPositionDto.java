package com.vz.backend.business.dto.hstl;

import java.io.Serializable;

import com.vz.backend.business.config.FolderPermissionEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class UserWithOrgAndPositionDto implements Serializable {
	private static final long serialVersionUID = 1L;

	private Long id;
	private String fullName;
	private String orgName;
	private String positionName;
	private FolderPermissionEnum permission;
	
	public UserWithOrgAndPositionDto(Long id, String fullName, String orgName, String positionName, FolderPermissionEnum permission) {
		super();
		this.id = id;
		this.fullName = fullName;
		this.orgName = orgName;
		this.positionName = positionName;
		this.permission = permission;
	}
}
