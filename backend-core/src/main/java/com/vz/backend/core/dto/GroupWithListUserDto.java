package com.vz.backend.core.dto;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class GroupWithListUserDto {
	private Long id;
	private String name;
	private String description;
	private Long nodeId;
	private List<Long> listUser;
}
