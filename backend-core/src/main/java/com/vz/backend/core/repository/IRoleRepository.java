package com.vz.backend.core.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.vz.backend.core.domain.Role;

/**
 * @author DucND
 * @date May 17, 2020
 */
@Repository
public interface IRoleRepository extends IRepository<Role> {

	Role findByClientIdAndName(Long clientId, String role);
	
	@Query("SELECT count(*)>0 FROM Role r INNER JOIN UserRole ur "
			+ "ON r.active is true AND ur.active is true AND ur.roleId=r.id AND r.clientId=:clientId "
			+ "WHERE ur.userId=:userId AND r.name IN :roleNames")
	boolean existUserIdByClientIdAndNamesInUserRole(@Param("userId") Long userId, @Param("clientId") Long clientId,
			@Param("roleNames") String[] roleNames);

	@Query("SELECT count(*)>0 FROM Role r INNER JOIN PositionRole pr "
			+ "ON pr.roleId = r.id AND r.clientId = :clientId AND pr.clientId = :clientId AND r.active = :active AND pr.active = :active"
			+ "INNER JOIN User u ON pr.posId = u.position AND u.active = :active AND u.clientId = :clientId "
			+ "WHERE u.id = :userId AND r.name IN :roleNames")
	boolean existUserIdByClientIdAndNamesInPositionRole(long userId, long clientId, String[] roleNames);

	@Query("SELECT count(*)>0 FROM Permission p INNER JOIN PositionRole pr "
			+ "ON p.roleId=pr.roleId AND pr.active is TRUE "
			+ "INNER JOIN User u ON u.id=:userId and u.position=pr.posId "
			+ "WHERE p.active is TRUE AND p.module.code=:code") // AND p.clientId=:clientId
	boolean existUserInModuleByPosition(Long userId, String code/* , Long clientId */);

	@Query("SELECT count(*)>0 FROM Permission p INNER JOIN UserRole ur "
			+ "ON p.roleId=ur.roleId AND ur.active is TRUE AND ur.userId=:userId "
			+ "WHERE p.active is TRUE AND p.module.code=:code")
	boolean existUserInModuleByUser(Long userId, String code);

	@Query("SELECT count(*)>0 FROM Permission p WHERE p.active is TRUE AND p.clientId = :clientId AND p.roleId = :currentRole AND p.module.code IN (:code)")
	boolean existRoleByModuleCode(long currentRole, List<String> code, long clientId);

	@Query("SELECT DISTINCT r FROM Role r WHERE r.id IN "
			+ "(SELECT p.roleId FROM Permission p INNER JOIN UserRole ur ON ur.active = :active AND p.roleId = ur.roleId AND ur.userId = :userId "
			+ "WHERE p.active = :active AND p.clientId = :clientId AND p.module.code = :code) "
			+ "OR r.id IN (SELECT p1.roleId FROM Permission p1 INNER JOIN PositionRole pr ON pr.active = :active AND p1.roleId = pr.roleId AND pr.posId = :posId "
			+ "WHERE p1.active = :active AND p1.clientId = :clientId AND p1.module.code = :code)")
	List<Role> getRoleHaveModuleByUserAndActiveAndClientId(String code, long userId, long posId, boolean active,
			Long clientId);

	List<Role> findByClientIdAndCabinetTrue(Long clientId);

	Role findByIdAndClientIdAndActiveTrue(Long currentRole, Long clientId);
	
	@Query("SELECT r FROM Role r WHERE r.clientId = :clientId AND r.name = :name")
	List<Role> findRoleByName(Long clientId, String name);

	@Query("SELECT r FROM Role r WHERE r.name = :name AND r.cabinet is null")
	Role findByNameOfficeSite(String name);

	Role findByNameAndCabinetTrue(String name);

	List<Role> findByClientIdAndCabinetIsNull(Long clientId);
}
