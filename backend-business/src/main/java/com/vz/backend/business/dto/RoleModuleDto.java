package com.vz.backend.business.dto;

import java.util.List;

import com.vz.backend.core.domain.Module;

import lombok.Data;

@Data
public class RoleModuleDto {
	private long roleId;
	private List<Module> listModule;
}
