package com.vz.backend.core.repository;

import com.vz.backend.core.dto.OrgGroupDto;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.vz.backend.core.domain.Group;
import com.vz.backend.core.dto.GroupWithListUserInfoDto;

import java.util.List;

@Repository
public interface IGroupRepository extends IRepository<Group> {

	@Query("SELECT new com.vz.backend.core.dto.GroupWithListUserInfoDto(g.id, g.active, g.name, g.description) FROM Group g "
			+ "WHERE g.createBy = :userId AND g.clientId = :clientId AND (:active is null OR g.active = :active) AND (:groupName is null OR g.name like %:groupName%) "
			+ "AND (:nodeId is null OR g.nodeId = :nodeId) AND (:description is null OR g.description like %:description%)")
	Page<GroupWithListUserInfoDto> getAllGroupByUserId(Long userId, Boolean active, String groupName, String description, Long nodeId, Long clientId, Pageable pageable);

	@Modifying
	@Query("DELETE FROM Group g WHERE g.id = :groupId AND g.createBy = :userId AND g.clientId = :clientId")
	void deleteByGroupIdAndCreateByAndClientId(Long groupId, Long userId, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.OrgGroupDto(o.name, o.id) FROM Group o "
			+ "WHERE(:name is null or lower(o.name) like %:name% ) "
			+ "AND o.clientId =:clientId"
			+ " AND o.active=TRUE "
			+ " AND o.createBy=:userId "
			+ "ORDER BY o.name ASC")
	List<OrgGroupDto> searchName(String name, Long clientId, Long userId);

	@Query("SELECT NEW com.vz.backend.core.dto.OrgGroupDto(g.name, g.id) FROM Group g WHERE g.clientId = :clientId AND g.id IN :groupIds AND g.active IS TRUE")
	List<OrgGroupDto> findByClientIdAndIdInAndActiveTrue(Long clientId, List<Long> groupIds);
}
