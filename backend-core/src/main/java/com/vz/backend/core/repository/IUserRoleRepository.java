package com.vz.backend.core.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.vz.backend.core.domain.Role;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.domain.UserRole;
import com.vz.backend.core.dto.UserBasicDto;

/**
 * @author DucND
 * @date May 07, 2020
 */
@Repository
public interface IUserRoleRepository extends IRepository<UserRole> {
	
	@Query(value = "SELECT ur FROM UserRole ur WHERE ur.role.id=:roleId and ur.user.active=true and ur.clientId=:clientId")
	List<UserRole> findByRoleId(long roleId, Long clientId);

	@Query(value = "SELECT ur.user FROM UserRole ur WHERE ur.role.id=:roleId and ur.user.active=true and ur.active=:active and ur.clientId=:clientId order by ur.user.fullName")
	List<User> findByRoleIdAndActive(@Param(value = "roleId") long roleId, @Param(value = "active") boolean active,
			@Param(value = "clientId") Long clientId);

	@Query(value = "SELECT DISTINCT ur.role FROM UserRole ur WHERE ur.user.id=:userId and ur.user.active=true and ur.active=:active and ur.role.active=true and ur.clientId=:clientId order by ur.role.name")
	List<Role> findRoleByUserIdAndActive(@Param(value = "userId") Long userId, @Param(value = "active") boolean active,
			@Param(value = "clientId") Long clientId);

	@Query(value = "SELECT DISTINCT ur.roleId FROM UserRole ur WHERE ur.userId=:userId and ur.user.active=true and ur.active=:active and ur.role.active=true and ur.clientId=:clientId")
	List<Long> findRoleIdByUserIdAndActive(@Param(value = "userId") Long userId,
			@Param(value = "active") boolean active, @Param(value = "clientId") Long clientId);

	@Query(value = "SELECT ur FROM UserRole ur WHERE ur.role.id=:roleId and ur.user.id=:userId and ur.active=:active and ur.clientId =:clientId order by ur.user.fullName")
	UserRole findByRoleIdAndUserIdAndActive(@Param(value = "roleId") long roleId, @Param(value = "userId") long userId,
			@Param(value = "active") boolean active, @Param(value = "clientId") Long clientId);

	@Query(value = "SELECT ur.role FROM UserRole ur WHERE ur.user.userName=:userName and ur.clientId=:clientId")
	List<Role> findRoleByUserName(@Param(value = "userName") String userName, @Param(value = "clientId") Long clientId);

	@Query(value = "SELECT ur FROM UserRole ur WHERE ur.userId in (:userIds) and ur.roleId =:roleId and ur.clientId =:clientId and ur.active = true")
	List<UserRole> findRoleByListUserId(List<Long> userIds, Long roleId, Long clientId);

	@Query("SELECT DISTINCT NEW com.vz.backend.core.dto.UserBasicDto(u) FROM User u "
			+ " WHERE u.clientId =:clientId AND u.active = TRUE"
			+ " AND (u.id IN "
			+ " (SELECT ur.userId FROM UserRole ur WHERE ur.active = TRUE AND ur.clientId = :clientId AND ur.role.active = TRUE AND ur.role.clientId = :clientId AND ur.role.name = :name)"
			+ " OR u.position IN "
			+ " (SELECT pr.posId FROM PositionRole pr WHERE pr.active = TRUE AND pr.clientId = :clientId AND pr.role.active = TRUE AND pr.role.clientId = :clientId AND pr.role.name = :name))"
			+ " AND u.id NOT IN (SELECT s.userId FROM Secretary s WHERE s.active=TRUE AND s.clientId=:clientId)"
			)
	List<UserBasicDto> getListUserByRole(String name, Long clientId);

	UserRole findByUserIdAndRoleId(Long userId, Long roleId);
}
