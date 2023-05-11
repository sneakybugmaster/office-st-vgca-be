package com.vz.backend.core.dto;

import com.vz.backend.core.common.BussinessCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@AllArgsConstructor
@NoArgsConstructor
public class IdName {
	private Long id;
	private String name;
	
	public void valids() {
		BussinessCommon.require("Tổ chức", this.id);
		BussinessCommon.require("Tên tổ chức", this.name);
	}
}
