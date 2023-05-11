package com.vz.backend.core.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Role;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.UserBasicDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.IService;
import com.vz.backend.core.service.PositionRoleService;
import com.vz.backend.core.service.RoleService;
import com.vz.backend.core.service.UserRoleService;

/**
 * @author DucND
 * @date May 07, 2020
 */
@RestController
@RequestMapping("/role")
public class RoleController extends BaseController<Role> {
	@Autowired
	private RoleService roleService;

	@Autowired
	private UserRoleService userRoleService;

	@Autowired
	private PositionRoleService posRoleService;

	@Override
	public IService<Role> getService() {
		// TODO Auto-generated method stub
		return roleService;
	}

	@Override
	@PostMapping("/add")
	public ResponseEntity<Role> create(@RequestBody Role module) {
		try {
			return new ResponseEntity<>(roleService.add(module), HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@PostMapping("/updateRole/{id}")
	public ResponseEntity<?> update(@PathVariable String id, @RequestBody Role input) {
		return new ResponseEntity<>(roleService.update(Long.valueOf(id), input), HttpStatus.OK);
	}

	@GetMapping("/getUserActiveByRole/{roleId}")
	public ResponseEntity<?> getUserActiveByRole(@PathVariable long roleId) {
		List<User> data = userRoleService.findUserByRoleIdAndActive(roleId, true);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@GetMapping("/getPositionActiveByRole/{roleId}")
	public ResponseEntity<?> getPositionActiveByRole(@PathVariable long roleId) {
		List<Category> data = posRoleService.findPositionByRoleIdAndActiveAndClientId(roleId, true,
				BussinessCommon.getClientId());
		return new ResponseEntity<>(data, HttpStatus.OK);
	}
	
	/**
	 * If cabinet has no record
	 * @return
	 */
	@GetMapping("/getAll/cabinet")
	public ResponseEntity<?> getAllCabinet() {
		List<Role> data = roleService.findByClientIdAndCabinetTrue(BussinessCommon.getClientId());
		if (BussinessCommon.isEmptyList(data)) {
			data = roleService.findByClientId(BussinessCommon.getClientId());
		}
		return new ResponseEntity<>(data, HttpStatus.OK);
	}
	
	/**
	 * Danh sách user được phân quyền thư kí
	 * @return
	 */
	@GetMapping("/users/secretary/list")
	public ResponseEntity<List<UserBasicDto>> getListUserBySecretaryRole() {
		return new ResponseEntity<>(userRoleService.getListUserByRole(Constant.THU_KI), HttpStatus.OK);
	}
	
	@Override
	@GetMapping("/getAll")
	public ResponseEntity<List<Role>> getByClientId() {
		List<Role> data = roleService.findByClientIdAndCabinetIsNull(BussinessCommon.getClientId());
		return new ResponseEntity<>(data, HttpStatus.OK);
	}
}
