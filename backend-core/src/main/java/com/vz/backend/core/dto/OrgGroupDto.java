package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class OrgGroupDto {
    private String fullName;
    private long id;
    
	public OrgGroupDto(Long id, String name) {
		super();
		this.id = id;
		this.fullName = name;
	}
    
}
