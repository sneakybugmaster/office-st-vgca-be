package com.vz.backend.core.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.Module;
import com.vz.backend.core.domain.Role;
import com.vz.backend.core.dto.ModuleDto;
import com.vz.backend.core.service.IService;
import com.vz.backend.core.service.ModuleService;
import com.vz.backend.core.service.RoleService;

@RestController
@RequestMapping("/module")
public class ModuleController {

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private RoleService roleService;

	public IService<Module> getService() {
		return moduleService;
	}

	@PostMapping("/add")
	public ResponseEntity<Module> create(@RequestBody Module module) {
		try {
			return new ResponseEntity<>(getService().save(module), HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/getAllModules")
	public ResponseEntity<List<ModuleDto>> getAllModules() {
		try {
			return new ResponseEntity<>(moduleService.getTreeModulesByParentIdAndClientId(null, BussinessCommon.getClientId()), HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/updateOrderNumberById")
	public ResponseEntity<?> updateOrderNumberById(@RequestParam Long moduleId, @RequestParam Long orderNumber, @RequestParam(required = false) Boolean hide) {
		return new ResponseEntity<>(moduleService.updateOrderNumberById(moduleId, orderNumber, hide), HttpStatus.OK);
	}

	@PostMapping("/updateOrderNumber")
	public ResponseEntity<?> updateOrderNumber(@RequestBody List<ModuleDto> treeModules) {
		return new ResponseEntity<>(moduleService.updateOrderNumber(treeModules), HttpStatus.OK);
	}

	@GetMapping("/getByClient")
	public ResponseEntity<List<Module>> getFull() {
		try {
			return new ResponseEntity<>(
					moduleService.findByClientIdAndParentId(BussinessCommon.getClientId()), HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/getModules/{roleId}")
	public ResponseEntity<?> getModules(@PathVariable Long roleId) {
		Optional<Role> r = roleService.findById(roleId);
		if (r.isPresent()) {
			List<Role> lr = new ArrayList<>();
			lr.add(r.get());
			List<Module> moduleOfUser = moduleService.findByRoleList(lr);
			return new ResponseEntity<>(moduleOfUser, HttpStatus.OK);
		}
		return new ResponseEntity<>(new ArrayList<>(), HttpStatus.NOT_FOUND);
	}

	@GetMapping("/getScreen/{id}")
	public ResponseEntity<?> getScreen(@PathVariable Long id) {
		Module screen = moduleService.findByActiveAndId(true, id);
		return new ResponseEntity<>(screen, HttpStatus.OK);
	}
	
	@PostMapping("/add/usage")
	public ResponseEntity<Module> addUsageModule(@RequestBody Module module) {
		try {
			return new ResponseEntity<>(moduleService.addUsageModule(module), HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}
}
