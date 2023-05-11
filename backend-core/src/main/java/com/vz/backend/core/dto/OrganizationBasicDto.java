package com.vz.backend.core.dto;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class OrganizationBasicDto {
	private Long id;
	private String name;
	private List<UserBasicDto> users = new ArrayList<>();
	private List<OrganizationBasicDto> children = new ArrayList<>();
	@JsonIgnore
	private Long parentId;
	
	@JsonIgnore
	private String orgTypeModelName;
	
	@JsonIgnore
	private Integer order;
	
	public OrganizationBasicDto(Long id, String name) {
		super();
		this.id = id;
		this.name = name;
	}
	
	public OrganizationBasicDto(Long id, String name, Long parentId, String orgTypeModelName) {
		this(id, name);
		this.parentId = parentId;
		this.orgTypeModelName = orgTypeModelName;
	}
}
