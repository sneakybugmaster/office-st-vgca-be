package com.vz.backend.core.dto;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class GroupWithListUserInfoDto {
	private Long id;
	private Boolean active;
	private String name;
	private String description;
	private List<UserInfoDto> listUser;
	
	public GroupWithListUserInfoDto(Long id, Boolean active, String name, String description) {
		super();
		this.id = id;
		this.active = active;
		this.name = name;
		this.description = description;
	}
	
}
