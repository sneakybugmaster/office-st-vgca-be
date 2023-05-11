package com.vz.backend.core.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.PositionRole;
import com.vz.backend.core.domain.Role;

@Repository
public interface IPositionRoleRepository extends IRepository<PositionRole> {

	@Query(value = "SELECT pr.position FROM PositionRole pr WHERE pr.role.id = :roleId AND (:active is null OR pr.active=:active) AND pr.clientId = :clientId")
	List<Category> findPositionByRoleIdAndActiveAndClientId(long roleId, Boolean active, long clientId);

	@Query(value = "SELECT DISTINCT pr.posId FROM PositionRole pr WHERE pr.role.id = :roleId AND (:active is null OR pr.active=:active) AND pr.clientId = :clientId")
	List<Long> findPosIdByRoleIdAndActiveAndClientId(long roleId, Boolean active, long clientId);

	PositionRole findByRoleIdAndPosIdAndActiveAndClientId(long roleId, long posId, Boolean active, long clientId);

	@Query(value = "SELECT pr.role FROM PositionRole pr WHERE pr.posId = :posId AND (:active is null OR pr.active=:active) AND pr.clientId = :clientId")
	List<Role> findRoleByPosIdAndActiveAndClientId(long posId, Boolean active, long clientId);

	@Query(value = "SELECT pr.role FROM PositionRole pr JOIN User u ON pr.posId = u.position WHERE pr.active = :active AND u.active = :active AND u.id = :userId AND pr.clientId = :clientId AND u.clientId = :clientId ORDER BY pr.role.name")
	List<Role> findRoleByUserIdAndActiveAndClientId(long userId, boolean active, long clientId);

	@Query(value = "SELECT DISTINCT pr.roleId FROM PositionRole pr JOIN User u ON pr.posId = u.position WHERE pr.active = :active AND u.active = :active AND u.id = :userId AND pr.clientId = :clientId AND u.clientId = :clientId")
	List<Long> findRoleIdByUserIdAndActiveAndClientId(long userId, boolean active, long clientId);

	@Query(value = "SELECT pr.role FROM PositionRole pr JOIN User u ON pr.posId = u.position WHERE pr.active = :active AND u.active = :active AND u.userName = :userName AND pr.clientId = :clientId AND u.clientId = :clientId ORDER BY pr.role.name")
	List<Role> findRoleByUserNameAndActiveAndClientId(String userName, boolean active, long clientId);

	@Query(value = "SELECT DISTINCT pr.role FROM PositionRole pr JOIN User u ON pr.posId = u.position WHERE pr.active = :active AND u.active = :active AND u.id = :userId AND pr.roleId = :roleId AND pr.clientId = :clientId AND u.clientId = :clientId ORDER BY pr.role.name")
	Role findRoleByUserIdAndRoleIdAndActiveAndClientId(long userId, long roleId, boolean active, long clientId);
}
