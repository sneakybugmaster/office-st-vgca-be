package com.vz.backend.core.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.core.domain.Permission;
import com.vz.backend.core.domain.Role;
import com.vz.backend.core.repository.IPermissionRepository;
import com.vz.backend.core.repository.IRepository;

/**
 * @author DucND
 * @date May 07, 2020
 */
@Service
public class PermissionService extends BaseService<Permission> {

	@Autowired
	private IPermissionRepository permissionRepository;

	@Override
	public IRepository<Permission> getRepository() {
		return permissionRepository;
	}

	public List<Permission> findActiveByClientAndRole(Long clientId, List<Role> roleList) {
		return permissionRepository.findByClientAndRole(clientId, roleList, true);
	}

	public Permission findByRoleIdAndModuleId(Long roleId, Long moduleId) {
		return permissionRepository.findByRoleIdAndModuleId(roleId, moduleId);
	}

}
