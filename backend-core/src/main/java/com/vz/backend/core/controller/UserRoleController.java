package com.vz.backend.core.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.core.domain.UserRole;
import com.vz.backend.core.service.IService;
import com.vz.backend.core.service.UserRoleService;

/**
 * @author DucND
 * @date May 07, 2020
 */
@RestController
@RequestMapping("/config-role")
public class UserRoleController extends BaseController<UserRole> {

	@Autowired
	private UserRoleService userRoleService;

	@Override
	public IService<UserRole> getService() {
		// TODO Auto-generated method stub
		return userRoleService;
	}
}
