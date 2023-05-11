package com.vz.backend.core.service;

import java.util.Arrays;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.domain.Role;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.repository.IRoleRepository;

/**
 * @author DucND
 * @date May 07, 2020
 */
@Service
public class RoleService extends BaseService<Role> {

	@Autowired
	UserRoleService urService;

	@Autowired
	private UserService userService;

	@Autowired
	private IRoleRepository roleRepository;

	@Override
	public IRepository<Role> getRepository() {
		return roleRepository;
	}

	public Role findRoleByNameAndClientId(String role, Long clientId) {
		return roleRepository.findByClientIdAndName(clientId, role);
	}

	public boolean isVanThuVBDen(User user) {
		return userService.checkUserIdByModuleCodeAndClientId(user.getId(),
				Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName()), user.getClientId());
	}

	public boolean isVanThuVBDenByOrg(User user, long org) {
		return userService.checkUserIdByModuleCodeAndOrgAndClientId(user.getId(), org,
				Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName()), user.getClientId());
	}

	public boolean isVanThuVBDi(User user) {
		return userService.checkUserIdByModuleCodeAndClientId(user.getId(),
				Arrays.asList(ModuleCodeEnum.DRAFT_ISSUED.getName()), user.getClientId());
	}

	public boolean isVanThuVBDiByOrg(User user, long org) {
		return userService.checkUserIdByModuleCodeAndOrgAndClientId(user.getId(), org,
				Arrays.asList(ModuleCodeEnum.DRAFT_ISSUED.getName()), user.getClientId());
	}

	public boolean existUserInModule(String code) {
		User user = BussinessCommon.getUser();
		if (user.getCurrentRole() == null || user.getCurrentRole().equals(0L)) {
			if (roleRepository.existUserInModuleByUser(user.getId(), code)) {
				return true;
			}
			return roleRepository.existUserInModuleByPosition(user.getId(), code);
		}
		return roleRepository.existRoleByModuleCode(user.getCurrentRole(), Arrays.asList(code),
				BussinessCommon.getClientId());
	}

	public List<User> getListVanThuVBDenByOrg(Long orgId) {
		return userService.getListUserByModuleCodeAndOrgAndClientId(
				Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName()), orgId, BussinessCommon.getClientId());
	}

	public List<Long> getListIdsVanThuVBDenByOrg(Long orgId) {
		return userService.getListUserIdByModuleCodeAndOrgAndClientId(
				Arrays.asList(ModuleCodeEnum.DOC_OUT_LIST.getName()), orgId, BussinessCommon.getClientId());
	}

	public List<Role> getRoleHaveModuleByUser(ModuleCodeEnum code, User user) {
		return roleRepository.getRoleHaveModuleByUserAndActiveAndClientId(code.getName(), user.getId(),
				user.getPosition(), true, BussinessCommon.getClientId());
	}

	public boolean isSupervisor(User user) {
		return userService.checkUserIdByModuleCodeAndClientId(user.getId(),
				Arrays.asList(ModuleCodeEnum.TRACK_DOC_IN.getName(), ModuleCodeEnum.TRACK_DOC_OUT.getName()),
				user.getClientId());
	}

	public boolean isAllowCalendarModule(User user) {
		return userService.checkUserIdByModuleCodeAndClientId(user.getId(),
				Arrays.asList(ModuleCodeEnum.CAL_REGISTER.getName()), user.getClientId());
	}
	
	public boolean isAllowModule(User user, String... module) {
		return userService.checkUserIdByModuleCodeAndClientId(user.getId(),
				Arrays.asList(module), user.getClientId());
	}
	
	public boolean checkCanRetake(Long orgId, DocumentTypeEnum type) {
		if (orgId == null) {
			throw new RestExceptionHandler("Dữ liệu database sai, liên hệ team phát triển để sửa lỗi :D");
		}
		if (DocumentTypeEnum.VAN_BAN_DI.equals(type)) {
			return userService.checkUserIdByModuleCodeAndOrgAndClientId(BussinessCommon.getUserId(), orgId,
					Arrays.asList(ModuleCodeEnum.DOC_OUT_RETAKE.getName()), BussinessCommon.getClientId());
		} else {
			return userService.checkUserIdByModuleCodeAndOrgAndClientId(BussinessCommon.getUserId(), orgId,
					Arrays.asList(ModuleCodeEnum.DOC_IN_RETAKE.getName()), BussinessCommon.getClientId());
		}
	}

	public List<Role> findByClientIdAndCabinetTrue(Long clientId) {
		return roleRepository.findByClientIdAndCabinetTrue(clientId);
	}

	public Role findByIdAndClientIdAndActiveTrue(Long currentRole, Long clientId) {
		return roleRepository.findByIdAndClientIdAndActiveTrue(currentRole, clientId);
	}
	
	public boolean isRoleExist(String name) {
		Role roleInfo = roleRepository.findByClientIdAndName(BussinessCommon.getClientId(), name);
		if (roleInfo != null) {
			throw new RestExceptionHandler(Message.EXIST_ROLE_NAME);
		}
		return true;
	}
	
	@Override
	public Role add(Role role) {
		isRoleExist(role.getName());
		return roleRepository.save(role);
	}
	
	public Role update(Long id, Role role) {
		Role data = roleRepository.findByClientIdAndId(BussinessCommon.getClientId(), id);
		if (data == null)
			throw new RestExceptionHandler(Message.ROLE_NOT_FOUND);

		if (!data.getName().equals(role.getName())) {
			isRoleExist(role.getName());
			data.setName(role.getName());
		}
		
		return roleRepository.save(data);
	}

	public List<Role> findByClientIdAndCabinetIsNull(Long clientId) {
		return roleRepository.findByClientIdAndCabinetIsNull(clientId);
	}
}
