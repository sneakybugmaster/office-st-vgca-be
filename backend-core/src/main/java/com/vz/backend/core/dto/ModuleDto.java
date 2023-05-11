package com.vz.backend.core.dto;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class ModuleDto {
	private Long id;
	private String name;
	private Long orderNumber;
	private Long parentId;
	private Boolean hide;
	private String code;
	private List<ModuleDto> subModule;
	
	public ModuleDto(Long id, String name, Long orderNumber, Long parentId, Boolean hide, String code) {
		super();
		this.id = id;
		this.name = name;
		this.orderNumber = orderNumber;
		this.parentId = parentId;
		this.hide = hide;
		this.code = code;
	}
	
	public ModuleDto(Long id, String name, Integer orderNumber, Long parentId, Boolean hide) {
		super();
		this.id = id;
		this.name = name;
		this.orderNumber = orderNumber == null ? 0 :Long.parseLong(String.valueOf(orderNumber));
		this.parentId = parentId;
		this.hide = hide;
	}
	
	public Boolean getHide() {
		return this.hide == null ? false : this.hide;
	}
}
