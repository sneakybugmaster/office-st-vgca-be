package com.vz.backend.core.dto;

import java.io.Serializable;

import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class UserBasicDto implements Serializable {
	private static final long serialVersionUID = 1L;
	
	private Long id;
	private String fullName;
	private String orgName;
	private String positionName;
	private Long orgId;
	
	public UserBasicDto(Long id, String fullName, String positionName) {
		super();
		this.id = id;
		this.fullName = fullName;
		this.positionName = positionName;
	}
	
	public UserBasicDto(User user) {
		this.id = user.getId();
		this.fullName = user.getFullName();
		this.positionName = user.getPositionModel().getName();
		this.orgName = user.getOrgModel().getName();
	}
}
