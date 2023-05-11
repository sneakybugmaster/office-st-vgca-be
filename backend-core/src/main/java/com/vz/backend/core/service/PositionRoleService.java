package com.vz.backend.core.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.PositionRole;
import com.vz.backend.core.domain.Role;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.repository.IPositionRoleRepository;
import com.vz.backend.core.repository.IRepository;

@Service
public class PositionRoleService extends BaseService<PositionRole> {

	@Autowired
	private IPositionRoleRepository posRoleRepository;

	@Autowired
	private UserService userService;

	@Override
	public IRepository<PositionRole> getRepository() {
		return posRoleRepository;
	}

	public List<Category> findPositionByRoleIdAndActiveAndClientId(long roleId, Boolean active, long clientId) {
		return posRoleRepository.findPositionByRoleIdAndActiveAndClientId(roleId, active, clientId);
	}

	public PositionRole findByRoleIdAndPosIdAndActiveAndClientId(long roleId, long posId, Boolean active,
			long clientId) {
		return posRoleRepository.findByRoleIdAndPosIdAndActiveAndClientId(roleId, posId, active, clientId);
	}

	public List<Role> findRoleByPosIdAndActiveAndClientId(long posId, Boolean active, long clientId) {
		return posRoleRepository.findRoleByPosIdAndActiveAndClientId(posId, active, clientId);
	}

	public List<User> findUserByRoleIdAndActive(long roleId, boolean active) {
		List<Long> listPosId = posRoleRepository.findPosIdByRoleIdAndActiveAndClientId(roleId, active,
				BussinessCommon.getClientId());
		List<User> users = userService.findByPositionInAndActiveAndClientIdOrderByFullName(listPosId, true,
				BussinessCommon.getClientId());
		return users;
	}

	public List<Role> findRoleByUserIdAndActiveAndClientId(long userId, boolean active, long clientId) {
		return posRoleRepository.findRoleByUserIdAndActiveAndClientId(userId, active, clientId);
	}

	public List<Long> findRoleIdByUserIdAndActiveAndClientId(long userId, boolean active, long clientId) {
		return posRoleRepository.findRoleIdByUserIdAndActiveAndClientId(userId, active, clientId);
	}

	public List<Role> findRoleByUserNameAndActiveAndClientId(String userName, boolean active, Long clientId) {
		return posRoleRepository.findRoleByUserNameAndActiveAndClientId(userName, active, clientId);
	}

	public Role findRoleByUserIdAndRoleIdAndActiveAndClientId(long userId, long roleId, boolean active, long clientId) {
		return posRoleRepository.findRoleByUserIdAndRoleIdAndActiveAndClientId(userId, roleId, active, clientId);
	}
}
