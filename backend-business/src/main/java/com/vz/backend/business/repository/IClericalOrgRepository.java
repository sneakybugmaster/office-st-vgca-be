package com.vz.backend.business.repository;

import java.util.List;
import java.util.Set;

import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.ClericalOrg;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IClericalOrgRepository extends IRepository<ClericalOrg> {

	List<ClericalOrg> findByUserIdAndClientId(Long userId, Long clientId);

	@Query("SELECT DISTINCT c.orgId FROM ClericalOrg c WHERE c.userId = :userId AND c.clientId = :clientId AND c.active = :active")
	Set<Long> findOrgIdByUserIdAndClientIdAndActive(Long userId, Long clientId, boolean active);

	@Query("SELECT c.orgId FROM ClericalOrg c WHERE c.userId = :userId AND (:active is null OR c.active = :active) AND c.clientId = :clientId")
	List<Long> getClericalOrgByUserIdAndClientId(Long userId, Boolean active, Long clientId);

	@Query("SELECT count(1) > 0 FROM ClericalOrg c WHERE c.userId = :userId AND c.orgId = :orgId AND c.clientId = :clientId AND c.active = TRUE")
	boolean isClericalOrg(Long userId, Long orgId, Long clientId);
	
	@Query("SELECT DISTINCT c.userId FROM ClericalOrg c WHERE c.orgId = :orgId AND c.active = TRUE AND c.clientId = :clientId")
	List<Long> getClericalOrgByOrgIdAndClientId(Long orgId, Long clientId);

	@Query("select o from Organization o where o.id in (SELECT c.orgId FROM ClericalOrg c WHERE c.userId = :userId AND " +
			"(:active is null OR c.active = :active) AND c.clientId = :clientId) ORDER BY o.level ASC")
	List<Organization> getClericalOrgByListId(Long userId, Boolean active, Long clientId);

	@Query("SELECT u FROM ClericalOrg c inner join User u on u.id = c.userId WHERE c.orgId = :orgId AND c.active = TRUE AND c.clientId = :clientId AND u.active = TRUE")
	List<User> getClericalByOrgIdAndClientId(Long orgId, Long clientId);
}
