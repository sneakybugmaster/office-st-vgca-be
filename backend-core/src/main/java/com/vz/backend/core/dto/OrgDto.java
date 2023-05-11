package com.vz.backend.core.dto;

import com.vz.backend.core.common.BussinessCommon;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class OrgDto {
	private Long id;
	private String name;
	private String phone;
	private String address;
	private Long orgType;
	private String email;
	private Boolean active;
	private Long parentId;

	public OrgDto convert(OrgDto dto) {
		dto.address = BussinessCommon.convert(dto.getAddress());
		dto.phone = BussinessCommon.convert(dto.getPhone());
		dto.name = BussinessCommon.convert(dto.getName());
		dto.email = BussinessCommon.convert(dto.getEmail());
		dto.orgType = BussinessCommon.convert(dto.getOrgType());
		dto.parentId = BussinessCommon.convert(dto.getParentId());
		dto.id = BussinessCommon.convert(dto.getId());
		return dto;
	}
}
