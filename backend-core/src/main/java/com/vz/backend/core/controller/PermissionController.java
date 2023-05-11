package com.vz.backend.core.controller;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

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
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.SystemEnum;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Module;
import com.vz.backend.core.domain.Permission;
import com.vz.backend.core.domain.PositionRole;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.domain.UserRole;
import com.vz.backend.core.repository.IPermissionRepository;
import com.vz.backend.core.service.ModuleService;
import com.vz.backend.core.service.PermissionService;
import com.vz.backend.core.service.PositionRoleService;
import com.vz.backend.core.service.UserRoleService;
import com.vz.backend.util.StringUtils;

/**
 * @author DucND
 * @date May 07, 2020
 */
@RestController
@RequestMapping("/permission")
public class PermissionController {

	@Autowired
	private PermissionService permissionService;

	@Autowired
	private IPermissionRepository permissionRepository;

	@Autowired
	private ModuleService moduleService;

	@Autowired
	private PositionRoleService posRoleService;

	@Autowired
	private UserRoleService userRoleService;

	@GetMapping("/inactiveAuthorizeUser/{roleId}/{userId}")
	public ResponseEntity<?> active(@PathVariable long roleId, @PathVariable long userId) {
		try {
			UserRole roleDetail = userRoleService.findByRoleIdAndUserIdAndActive(roleId, userId, true);
			if (roleDetail != null) {
				roleDetail.setActive(false);
				userRoleService.save(roleDetail);
			}
			List<User> data = userRoleService.findUserByRoleIdAndActive(roleId, true);
			return new ResponseEntity<>(data, HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/inactiveAuthorizePosition/{roleId}/{posId}")
	public ResponseEntity<?> inactiveAuthorizePosition(@PathVariable long roleId, @PathVariable long posId) {
		try {
			PositionRole roleDetail = posRoleService.findByRoleIdAndPosIdAndActiveAndClientId(roleId, posId, true,
					BussinessCommon.getClientId());
			if (roleDetail != null) {
				roleDetail.setActive(false);
				posRoleService.save(roleDetail);
			}
			List<Category> data = posRoleService.findPositionByRoleIdAndActiveAndClientId(roleId, true,
					BussinessCommon.getClientId());
			return new ResponseEntity<>(data, HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@PostMapping("/addAuthorizeUserList/{roleId}")
	public ResponseEntity<?> createMulti(@PathVariable long roleId, @RequestBody List<User> userList) {
		try {
			// Target description: Insert users to roleId
			// Step1: Only insert user does not exist in roleId
			// Step2: Return all user of roleId

			List<UserRole> listData = new ArrayList<>();
			List<UserRole> userOfRole = userRoleService.findUserByRoleId(roleId);
			Map<Long, UserRole> map = new HashMap<>();
			userOfRole.forEach(i -> {
				map.put(i.getUserId(), i);
			});

			if (!BussinessCommon.isEmptyList(userList)) {
				Set<Long> newUserIds = userList.stream().filter(i -> i.getId() != null).map(User::getId)
						.collect(Collectors.toSet());
				for (Long i : newUserIds) {
					UserRole value = map.get(i);
					if (map.containsKey(i)) {
						// re-open
						if (!Boolean.TRUE.equals(value.getActive())) {
							value.setActive(true);
							listData.add(value);
						}
					} else {
						// add new
						value = new UserRole();
						value.setRoleId(roleId);
						value.setUserId(i);
						listData.add(value);
					}
				}
				listData = userRoleService.saveAll(listData);
			}
			List<User> response = userRoleService.findUserByRoleIdAndActive(roleId, true);
			return new ResponseEntity<>(response, HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@PostMapping("/addAuthorizePositionList/{roleId}")
	public ResponseEntity<?> addAuthorizePositionList(@PathVariable long roleId, @RequestBody List<Category> posList) {
		try {
			List<PositionRole> listData = new ArrayList<>();
			List<Category> posOfRole = posRoleService.findPositionByRoleIdAndActiveAndClientId(roleId, true,
					BussinessCommon.getClientId());
			if (posList != null && posList.size() > 0) {
				PositionRole item;
				// List<Category> posOfRole =
				// posRoleService.findPositionByRoleIdAndActiveAndClientId(roleId, true,
				// BussinessCommon.getClientId());
				for (Category element : posList) {
					if (posOfRole != null && posOfRole.size() > 0) {
						for (int j = 0; j < posOfRole.size(); j++) {
							if (posOfRole.get(j).getId().equals(element.getId())) {
								break;
							}
							// last item
							if (j == posOfRole.size() - 1) {
								item = new PositionRole();
								item.setRoleId(roleId);
								item.setActive(true);
								item.setPosId(element.getId());
								listData.add(item);
							}
						}
					} else {
						item = new PositionRole();
						item.setRoleId(roleId);
						item.setActive(true);
						item.setPosId(element.getId());
						listData.add(item);
					}
				}
				listData = posRoleService.saveAll(listData);
			}
			// List<User> response = userRoleService.findUserByRoleIdAndActive(roleId,
			// true);
			List<Category> response = posRoleService.findPositionByRoleIdAndActiveAndClientId(roleId, true,
					BussinessCommon.getClientId());
			return new ResponseEntity<>(response, HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@PostMapping("/updateAuthorizeModule/{roleId}")
	public ResponseEntity<?> create(@PathVariable long roleId, @RequestBody List<Module> modules) {
		try {
			// Target description: Active/inactive module of role
			// Step1: Insert or update permission record.
			// Step2: Insert or update permission of module child (the sample module parent)

			if (modules != null && modules.size() > 0) {
				Permission item;
				for (Module module : modules) {
					item = permissionService.findByRoleIdAndModuleId(roleId, module.getId());
					if (item == null) {
						item = new Permission();
						item.setRoleId(roleId);
						item.setModuleId(module.getId());
					}
					if (module.getIsChecked() != null) {
						item.setActive(module.getIsChecked());
					} else {
						item.setActive(false);
					}
					
					if (Boolean.TRUE.equals(module.getIsParent())) {
						updateActivePermission(roleId, module.getSubModule(), null);
					}
					permissionService.save(item);
				}
			}
			List<Long> roleList = new ArrayList<>();
			roleList.add(roleId);
			List<Module> moduleOfRole = moduleService.findByRoleIdList(roleList, null);
			return new ResponseEntity<>(moduleOfRole, HttpStatus.OK);
		} catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.badRequest().build();
		}
	}
	
	/**
	 * Save permission cabinet module to redirect Office to Cabinet
	 * @param module
	 */
	private void savePermissionCabinetModule(Long roleId, Module module) {
		// Target : Active/inactive module cabinet of role
		// Step1: Get all module cabinet
		// Step2: Insert or update permission of module permission

		if (module == null || module.getIsChecked() == null || !Constant.CAL_CABINET_MEETING.equals(module.getCode())) {
			return;
		}

		List<Module> cabinetModule = moduleService.findByClientIdAndSiteAndActiveTrue(BussinessCommon.getClientId(),
				SystemEnum.CABINET);
		Permission tmp = null;
		for (Module i : cabinetModule) {
			tmp = permissionService.findByRoleIdAndModuleId(roleId, i.getId());
			if (tmp == null) {
				tmp = new Permission();
				tmp.setRoleId(roleId);
				tmp.setModuleId(i.getId());
			}

			tmp.setActive(module.getIsChecked());
			permissionService.save(tmp);
		}
	}
	
	/**
	 * Get office module corresponding cabinet module to add permission
	 * 
	 * @param modules
	 * @return
	 */
	private List<Module> getBuffModuleMappingOfficeSite(List<Module> modules) {
		HashMap<String, String> map = new HashMap<>();
		map.put(Constant.CABINET_ADMIN_USERS, ModuleCodeEnum.USER.name);
		map.put(Constant.CABINET_ADMIN_THREADS, ModuleCodeEnum.PROCESS.name);
		map.put(Constant.CABINET_ADMIN_ROLES, ModuleCodeEnum.ROLE.name);
		map.put(Constant.CABINET_ADMIN_ROOMS, ModuleCodeEnum.MEETING_ROOM.name);
		map.put(Constant.CABINET_ADMIN, ModuleCodeEnum.ADMIN.name);
		map.put(Constant.CABINET_GROUP, ModuleCodeEnum.GROUP_CONTACT.name);

		if (BussinessCommon.isEmptyList(modules))
			return Collections.emptyList();
		List<String> codes = modules.stream().filter(i -> i.getCode() != null).map(Module::getCode)
				.collect(Collectors.toList());
		List<Module> buff = new ArrayList<>();
		for (Module module : modules) {
			String key = module.getCode();
			boolean active = Boolean.TRUE.equals(module.getIsChecked()) ? true : false;
			if (StringUtils.isNullOrEmpty(key))
				continue;
			String value = map.containsKey(key) ? map.get(key) : null;
			switch (key) {
			case Constant.CABINET_ADMIN_USERS:
				this.addModuleBuffByCode(codes, value, map, buff, active);
				this.addModuleBuffByCode(codes, Constant.OFFICE_ADMIN, map, buff, active);
				this.addModuleBuffByCode(codes, Constant.OFFICE_ORG, map, buff, active);
				this.addModuleBuffByCode(codes, Constant.OFFICE_CATEGORY, map, buff, active);
				break;
			case Constant.CABINET_ADMIN_THREADS:
				this.addModuleBuffByCode(codes, value, map, buff, active);
				this.addModuleBuffByCode(codes, Constant.OFFICE_ADMIN, map, buff, active);
				break;
			case Constant.CABINET_ADMIN_ROLES:
				this.addModuleBuffByCode(codes, value, map, buff, active);
				this.addModuleBuffByCode(codes, Constant.OFFICE_ADMIN, map, buff, active);
				break;
			case Constant.CABINET_ADMIN_ROOMS:
				this.addModuleBuffByCode(codes, value, map, buff, active);
				this.addModuleBuffByCode(codes, Constant.OFFICE_CALENDAR, map, buff, active);
				break;
			case Constant.CABINET_ADMIN:
				this.addModuleBuffByCode(codes, value, map, buff, active);
				this.addModuleBuffByCode(codes, Constant.OFFICE_ADMIN, map, buff, active);
				break;
			case Constant.CABINET_GROUP:
				this.addModuleBuffByCode(codes, value, map, buff, active);
				this.addModuleBuffByCode(codes, Constant.OFFICE_ADMIN, map, buff, active);
			default:
				break;
			}
		}
		return buff;
	}
	
	/**
	 * Add module buff by code
	 * 
	 * @param codes  : to check distinct
	 * @param value  : buff module
	 * @param map    : map default
	 * @param buff   : result list
	 * @param active : true/ false to set active in permission
	 */
	private void addModuleBuffByCode(List<String> codes, String value, HashMap<String, String> map, List<Module> buff,
			boolean active) {
		Module tmp = null;
		if (value == null)
			return;
		if (!codes.contains(value)) {
			tmp = moduleService.getByCode(value);
			if (tmp != null) {
				tmp.setIsChecked(active);
				buff.add(tmp);
				codes.add(tmp.getCode());
			}
		}
	}
	
	@PostMapping("/updateAuthModule4Cabinet/{roleId}")
	public ResponseEntity<?> updateAuthModule4Cabinet(@PathVariable long roleId, @RequestBody List<Module> modules) {
		try {
			// Target description: Active/inactive module of role
			// Step1: Insert or update permission record.
			// Step2: Insert or update permission of module child (the sample module parent)
			
			if (modules != null && modules.size() > 0) {
				Permission item;
				for (Module module : modules) {
					item = permissionService.findByRoleIdAndModuleId(roleId, module.getId());
					if (item == null) {
						item = new Permission();
						item.setRoleId(roleId);
						item.setModuleId(module.getId());
					}
					if (module.getIsChecked() != null) {
						item.setActive(module.getIsChecked());
					} else {
						item.setActive(false);
					}
					
					if (Boolean.TRUE.equals(module.getIsParent())) {
						updateActivePermission(roleId, module.getSubModule(), SystemEnum.CABINET);
					}
					permissionService.save(item);
				}
			}
			List<Long> roleList = new ArrayList<>();
			roleList.add(roleId);
			List<Module> moduleOfRole = moduleService.findByRoleIdList(roleList, null);
			return new ResponseEntity<>(moduleOfRole, HttpStatus.OK);
		} catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.badRequest().build();
		}
	}

	private void updateActivePermission(long roleId, List<Module> modules, SystemEnum site) {

		// Add permission for office modules corresponding cabinet site
		if (SystemEnum.CABINET.equals(site)) {
			List<Module> buff = getBuffModuleMappingOfficeSite(modules);
			if (!BussinessCommon.isEmptyList(modules)) {
				modules.addAll(buff);
			}
		}
		
		if (modules != null && modules.size() > 0) {
			Permission item;
			for (Module module : modules) {
				
				// save permission cabinet module
				this.savePermissionCabinetModule(roleId, module);
				
				item = permissionRepository.findByRoleIdAndModuleId(roleId, module.getId());
				if (item == null) {
					item = new Permission();
					if (module.getIsChecked() != null) {
						item.setActive(module.getIsChecked());
					} else {
						item.setActive(false);
					}
					item.setRoleId(roleId);
					item.setModuleId(module.getId());
				} else if (item != null) {
					if (module.getIsChecked() != null) {
						item.setActive(module.getIsChecked());
					} else {
						item.setActive(false);
					}
				}
				
				if (Boolean.TRUE.equals(module.getIsParent())) {
					updateActivePermission(roleId, module.getSubModule(), site);
				}
				permissionService.save(item);
			}
		}
	}

}
