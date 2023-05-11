package com.vz.backend.core.repository;

import java.util.List;
import java.util.Set;

import com.vz.backend.core.domain.User;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.core.config.AuthorityEnum;
import com.vz.backend.core.domain.AuthorityUser;
import com.vz.backend.core.dto.UserInfoDto;
import com.vz.backend.core.dto.UserTreeDto;

@Repository
public interface IAuthorityUserRepository extends IRepository<AuthorityUser> {
	@Query("SELECT a FROM AuthorityUser a WHERE a.active IS :active AND (:userId is null OR a.userId = :userId) AND a.clientId = :clientId AND (:positionId is null OR a.positionId=:positionId) ")
	List<AuthorityUser> findByUserIdAndClientIdAndActive(Long userId, Long positionId, Long clientId, boolean active);

	@Query("SELECT a.userId FROM AuthorityUser a WHERE a.active IS true AND a.authority = :autho AND a.user.org = :orgId AND a.clientId = :clientId")
	List<Long> getListUserApprovalByOrgIdAndAuthority(Long orgId, AuthorityEnum autho, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.UserTreeDto(u.id, u.org, u.lead, u.position, u.positionModel.name, u.positionModel.order, u.fullName, u.userName) "
			+ "FROM AuthorityUser a JOIN User u ON u.id = a.userId AND a.clientId = u.clientId AND a.active = u.active "
			+ "WHERE a.active IS TRUE AND a.authority = :authority AND a.clientId = :clientId")
	List<UserTreeDto> getByAuthority(AuthorityEnum authority, Long clientId);

	@Query("SELECT u "
			+ "FROM User u inner join Category ca on ca.id = u.position "
			+ "WHERE u.clientId=:clientId and ca.clientId=:clientId and u.active=true and ca.name  in (:list) " +
			"and (lower(u.fullName) like %:text% OR lower(u.userName) like %:text% OR lower(u.email) like %:text% ) and (:orgId is null or u.org = :orgId)")
	List<User> getListUserTBANDPTB(List<String> list, Long clientId,String text, Long orgId);
	
	@Query("SELECT new com.vz.backend.core.dto.UserInfoDto(u.id, u.fullName, u.positionModel.name, u.org) "
			+ "FROM User u INNER JOIN AuthorityUser a ON u.id = a.userId AND a.active is true "
			+ "WHERE a.authority = :authority AND u.active is true AND u.clientId = :clientId "
			+ "ORDER BY u.positionModel.order")
	List<UserInfoDto> getByAuthority1(AuthorityEnum authority, Long clientId);

	@Query("SELECT a.userId FROM AuthorityUser a WHERE a.active IS TRUE AND a.clientId =:clientId "
			+ "AND a.authority =:authority AND a.userId IN (:idUs) ")
	Set<Long> checkAuthorByIds(Set<Long> idUs, AuthorityEnum authority, Long clientId);

	@Query("SELECT a FROM AuthorityUser a WHERE a.active IS TRUE AND a.clientId =:clientId AND a.positionId IN (:positionIds) ")
	List<AuthorityUser> getByPositionIds(List<Long> positionIds, Long clientId);

	List<AuthorityUser> findByClientIdAndUserIdAndActiveTrue(Long clientId, Long userId);

	@Query("SELECT count(1)>0 FROM AuthorityUser a WHERE a.active IS true AND a.authority = :autho AND a.userId=:userId AND a.clientId = :clientId")
	Boolean checkAuthorByPermission(AuthorityEnum autho, Long clientId, Long userId);

}