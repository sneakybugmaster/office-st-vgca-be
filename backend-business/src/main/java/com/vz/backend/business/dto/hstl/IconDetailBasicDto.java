package com.vz.backend.business.dto.hstl;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class IconDetailBasicDto {
	private Long id;
	private String name;
	private Long totalItems;
	
	public IconDetailBasicDto(Long id, String name, Long totalItems) {
		super();
		this.id = id;
		this.name = name;
		this.totalItems = totalItems;
	}
}
