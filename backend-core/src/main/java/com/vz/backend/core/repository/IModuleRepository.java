package com.vz.backend.core.repository;

import com.vz.backend.core.config.SystemEnum;
import com.vz.backend.core.domain.Module;
import com.vz.backend.core.domain.Role;
import com.vz.backend.core.dto.ModuleDto;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IModuleRepository extends IRepository<Module> {

	@Query(value = "SELECT distinct p.module FROM Permission p WHERE p.role in :roleList and p.active=:active and p.module.active=:active AND (p.module.site IS NULL OR p.module.site = 'OFFICE') order by p.module.orderNumber DESC")
	List<Module> findByActiveAndRoleAndParentId(@Param(value = "roleList") List<Role> roleList,
			@Param(value = "active") Boolean active);

	@Query(value = "SELECT p.module FROM Permission p WHERE p.roleId in :roleList and p.active=:active and p.module.active=:active and ((:parentId is null and p.module.parentId is null) or p.module.parentId=:parentId) AND (p.module.site IS NULL OR p.module.site = 'OFFICE') order by p.module.orderNumber DESC")
	List<Module> findByActiveAndRoleIdAndParentId(@Param(value = "roleList") List<Long> roleList,
			@Param(value = "active") Boolean active, @Param(value = "parentId") Long parentId);

	@Query(value = "SELECT m FROM Module m WHERE m.active=:active and (m.isDefault=true or (m.isDefault=false and m.clientId=:clientId)) AND (m.site IS NULL OR m.site = 'OFFICE') order by m.name")
	List<Module> findByActiveAndClientId(@Param(value = "active") Boolean active,
			@Param(value = "clientId") Long clientId);

	@Query(value = "SELECT m FROM Module m WHERE m.active=:active and (m.isDefault=true or m.clientId=:clientId) AND (m.site IS NULL OR m.site = 'OFFICE') order by m.name")
	List<Module> findByClientIdOrDefault(Boolean active, Long clientId);

	List<Module> findByActiveAndParentId(Boolean active, Long parentId);

	Module findByActiveAndId(Boolean active, Long id);

	Module findByCode(String code);

	@Query("SELECT count(*)>0 FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.roleId = :roleId AND p.module.code = :code AND p.role.active = :active AND p.module.active = :active")
	boolean existModuleByRoleId(String code, long roleId, boolean active, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.ModuleDto(m.id, m.name, m.orderNumber, m.parentId, m.hide, m.code) "
			+ "FROM Module m WHERE (:active is null OR m.active = :active) AND (m.isDefault=true or (m.isDefault=false and m.clientId=:clientId)) "
			+ "AND ((:parentId is null and m.parentId is null) OR m.parentId=:parentId) AND (m.site IS NULL OR m.site = 'OFFICE') ORDER BY orderNumber DESC")
	List<ModuleDto> getAllModulesByParentIdAndClientId(Boolean active, Long parentId, Long clientId);

	@Modifying
	@Query("UPDATE Module m SET m.orderNumber = :orderNumber, m.hide = :hide WHERE m.id = :id AND m.clientId = :clientId")
	void updateOrderNumberByIdAndClientId(Long id, Long orderNumber, Boolean hide, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.ModuleDto(m.id, m.name, m.orderNumber, m.parentId, m.hide, m.code) "
			+ "FROM Module m WHERE (:active is null OR m.active = :active) AND (m.isDefault=true or (m.isDefault=false and m.clientId=:clientId)) AND (m.site IS NULL OR m.site = 'OFFICE') ORDER BY orderNumber DESC")
	List<ModuleDto> findModuleDtoByClientIdAndActive(Long clientId, boolean active);

	Module findFirstByCodeAndClientIdAndActive(String code, Long clientId, boolean active);

	@Query(value = "SELECT m FROM Module m "
			+ " INNER JOIN Module p ON m.parentId = p.id "
			+ " WHERE m.active = TRUE AND m.clientId = :clientId AND p.active = TRUE AND p.clientId=:clientId AND p.code =:code AND p.parentId IS NULL")
	List<Module> findByParentCode(String code, Long clientId);

	@Query(value = "SELECT m FROM Module m WHERE m.active = TRUE AND (m.clientId=:clientId) AND m.site = 'CABINET' ORDER BY m.name")
	List<Module> findAdminModuleForAdminCabinet(Long clientId);

	Module findFirstByCodeAndClientIdAndActiveTrue(String code, Long clientId);

	Module findByCodeAndSite(String code, SystemEnum cabinet);
	
	@Query("SELECT m FROM Module m WHERE m.code = :code AND m.site IS NULL")
	Module findByCodeOfficeSite(String code);

	List<Module> findByClientIdAndSiteAndActiveTrue(Long clientId, SystemEnum site);
	
	@Modifying
	@Query("UPDATE Module m SET m.active = FALSE WHERE m.code IN (:codes)")
	void inactiveModule(List<String> codes);
}
