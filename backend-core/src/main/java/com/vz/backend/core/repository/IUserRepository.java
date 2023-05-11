package com.vz.backend.core.repository;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.vz.backend.core.config.AuthorityEnum;
import com.vz.backend.core.domain.Role;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.ClericalWithOrgIds;
import com.vz.backend.core.dto.ContactDto;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.dto.LabelValueId;
import com.vz.backend.core.dto.SearchNameDto;
import com.vz.backend.core.dto.SignerDto;
import com.vz.backend.core.dto.UserBasicDto;
import com.vz.backend.core.dto.UserDto;
import com.vz.backend.core.dto.UserFullNamePositionName;
import com.vz.backend.core.dto.UserIdOrgIdDto;
import com.vz.backend.core.dto.UserInfoDto;
import com.vz.backend.core.dto.UserTreeDto;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Repository
public interface IUserRepository extends IRepository<User> {

	String QUERY_ELSE = " AND u.fullName IS NOT NULL AND u.userName <> 'admin' ";

	User findByUserNameAndActive(String userName, boolean active);

	User findByUserNameAndPasswordAndActive(String userName, String password, boolean active);

	User findByUserNameAndClientId(String userName, Long clientId);

	User findByIdAndClientId(Long id, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.UserIdOrgIdDto(u.id, u.org) FROM User u WHERE u.id in :ids and u.active is TRUE and u.clientId=:clientId")
	List<UserIdOrgIdDto> findIdAndOrgId(Long[] ids, Long clientId);

	@Query("SELECT u FROM User u where u.id in (:ids) and u.clientId = :clientId")
	List<User> findByListUserNameAndClientId(Long[] ids, Long clientId);

	List<User> findByOrgAndClientId(Long orgId, Long clientId);

	List<User> findByClientIdAndPosition(Long clientId, Long positionId);

	@Query("SELECT u FROM User u " + "WHERE (:fullName is null OR lower(u.fullName) like %:fullName%) "
			+ "AND (:userName is null OR lower(u.userName) like %:userName%) "
			+ "AND (:email is null OR lower(u.email) like %:email%) "
			+ "AND (:phone is null OR lower(u.phone) like %:phone%) " + "AND (:gender is null OR u.gender is :gender) "
			+ "AND (:indentity is null OR lower(u.indentity) like %:indentity%) "
			+ "AND (:title is null OR lower(u.title) like %:title%) "
			+ "AND (:nameToken is null OR lower(u.nameToken) like %:nameToken%) "
			+ "AND (:serialToken is null OR lower(u.serialToken) like %:serialToken%) "
			+ "AND (:employeeId is null OR u.employeeId = :employeeId) "
			+ "AND (:employeeCode is null OR lower(u.employeeCode) like %:employeeCode%) "
			+ "AND (:salt is null OR lower(u.salt) like %:salt%) " + "AND ((:org) is null OR u.org IN ( :org)) "
			+ "AND (:position is null OR u.position = :position) " + "AND (:lead is null OR u.lead is :lead) "
			+ "AND (:clientId is null or u.clientId = :clientId) "
			+ "AND (coalesce(:frBirthday, null) is null OR (u.birthday BETWEEN :frBirthday AND :toBirthday)) "
			+ "ORDER BY (CASE WHEN u.active is null THEN 3 WHEN u.active is FALSE THEN 2 ELSE 1 END) ASC, u.positionModel.order ASC")
	Page<User> findUser(@Param(value = "fullName") String fullName, @Param(value = "userName") String userName,
			@Param(value = "email") String email, @Param(value = "phone") String phone,
			@Param(value = "gender") Long gender, @Param(value = "indentity") String indentity,
			@Param(value = "title") String title, @Param(value = "nameToken") String nameToken,
			@Param(value = "serialToken") String serialToken,
			@Param(value = "employeeId") Long employeeId,
			@Param(value = "employeeCode") String employeeCode, @Param(value = "salt") String salt,
			@Param(value = "org") List<Long> org, @Param(value = "position") Long position,
			@Param(value = "lead") Boolean lead, @Param(value = "frBirthday") Date frBirthday,
			@Param(value = "toBirthday") Date toBirthday,
			@Param(value = "clientId") Long clientId, Pageable pageable);

	@Query("Select u from User u where (:clientId is null or u.clientId = :clientId)")
	Page<User> getAllUser(@Param(value = "clientId") Long clientId, Pageable pageable);

	@Query("Select new com.vz.backend.core.dto.UserInfoDto(u.id, u.fullName, u.userName, u.lead, u.title, u.position, u.org) from User u "
			+ "where (:clientId is null or u.clientId = :clientId) and u.lead = :lead")
	List<UserInfoDto> getAllUserByLead(Long clientId, Boolean lead);

	@Query("Select u from User u join Category c on u.position = c.id where (:textSearch is null OR (lower(u.fullName) like %:textSearch% "
			+ " OR lower(u.userName) like %:textSearch% OR lower(u.email) like %:textSearch%)) "
			+ "AND u.active=true AND u.clientId=:clientId ORDER BY c.order ASC")
	List<User> searchUserActive(@Param(value = "textSearch") String textSearch,
			@Param(value = "clientId") Long clientId);

	@Query("Select new com.vz.backend.core.dto.SignerDto(0, u.id, u.fullName, u.orgModel.name, u.positionModel.name, u.phone) "
			+ "from User u where (:textSearch is null or lower(u.fullName) like %:textSearch% " + " OR lower(u.userName) like %:textSearch% "
			+ "OR lower(u.email) like %:textSearch%) "
			+ "AND u.active=true AND u.clientId=:clientId ORDER BY coalesce(u.positionModel.order, 0), u.fullName")
	List<SignerDto> searchUserSign(@Param(value = "textSearch") String textSearch,
			@Param(value = "clientId") Long clientId);

	@Query("Select u.userName from User u where u.id = :id")
	String getUserNameById(@Param(value = "id") Long id);

	//	@Query("SELECT u FROM User u where u.userName in (:users) and u.clientId = :clientId")
	//	List<User> findByListUserIdAndClientId(String[] users, Long clientId);

	@Query("Select u.fullName from User u where u.id = :userId and u.clientId =:clientId")
	String getFullNameById(Long userId, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.UserDto(u.id, u.userName, u.fullName) FROM User u where u.clientId = :clientId and u.active=true")
	List<UserDto> getAllUserDtoByActive(Long clientId);

	@Query("SELECT u FROM User u WHERE u.id IN (:listIds)")
	List<User> findByIds(Long[] listIds);

	@Query("SELECT u.id FROM User u WHERE u.clientId = :clientId and u.active = :active")
	List<Long> getIdList(Long clientId, Boolean active);

	@Query("SELECT u FROM User u WHERE u.clientId = :clientId and u.active = :active and ((:listIds) is null or u.id in (:listIds))")
	List<User> findByIds(List<Long> listIds, Long clientId, Boolean active);

	@Query("SELECT u.id FROM User u WHERE u.org IN (:org) AND u.lead = true")
	List<Long> getListLeadUserIdByOrg(List<Long> org);

	@Query("SELECT u.userName FROM User u WHERE u.clientId = :clientId and lower(u.fullName) like %:key% and u.active = true")
	List<String> findUserNameByKeys(String key, Long clientId);

	@Query("SELECT u.id FROM User u WHERE u.clientId = :clientId and lower(u.fullName) like %:key% and u.active = true")
	List<Long> findIdByKeys(String key, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.UserTreeDto(u.id, u.org, u.positionModel, u.fullName, u.userName, u.lead) "
			+ "FROM User u WHERE u.clientId=:clientId AND u.active is TRUE "
			+ "ORDER BY coalesce(u.positionModel.order, 0), u.fullName")
	List<UserTreeDto> getAllOrder(Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.UserTreeDto(u.id, u.org, u.positionModel, u.fullName, u.userName, u.lead) "
			+ "FROM User u WHERE u.id IN "
			+ "(SELECT DISTINCT u1.id FROM User u1 LEFT JOIN PositionRole pr ON pr.active is true AND u1.position = pr.position "
			+ "LEFT JOIN UserRole ur ON ur.active is true AND u1.id = ur.userId "
			+ "INNER JOIN Permission p ON p.active is true AND (p.roleId = pr.roleId OR p.roleId = ur.roleId) "
			+ "WHERE 1=1 AND p.module.code = :code AND u1.clientId=:clientId AND u1.active is TRUE AND u1.fullName is not null "
			+ "GROUP BY u1.id) " + "ORDER BY coalesce(u.positionModel.order, 0), u.fullName")
	List<UserTreeDto> getAllOrder1(String code, Long clientId);

	List<User> findByPositionInAndActiveAndClientIdOrderByFullName(List<Long> listPos, boolean active, Long clientId);

	@Modifying()
	@Query("UPDATE User u SET u.currentRole = :roleId WHERE u.id = :userId and u.clientId=:clientId")
	void setCurrentRole(long userId, long roleId, Long clientId);

	@Query("SELECT count(*)>0 FROM User u WHERE u.id IN (SELECT u1.id FROM User u1 INNER JOIN UserRole ur ON u1.id = ur.userId AND ur.roleId = :roleId AND ur.active = :active AND ur.clientId = :clientId WHERE u1.active = :active AND u1.id = :userId) "
			+ "OR u.id IN (SELECT u2.id FROM User u2 INNER JOIN PositionRole pr ON u2.position = pr.posId AND pr.roleId = :roleId AND pr.active = :active AND pr.clientId = :clientId WHERE u2.active = :active AND u2.id = :userId)")
	boolean existUserByRoleIdAndActiveAndClientId(long userId, long roleId, boolean active, long clientId);

	@Query("SELECT u FROM User u WHERE u.id IN (SELECT u1.id FROM User u1 INNER JOIN UserRole ur ON u1.id = ur.userId AND ur.roleId = :roleId AND ur.active = :active AND ur.clientId = :clientId WHERE u1.active = :active) "
			+ "OR u.id IN (SELECT u2.id FROM User u2 INNER JOIN PositionRole pr ON u2.position = pr.posId AND pr.roleId = :roleId AND pr.active = :active AND pr.clientId = :clientId WHERE u2.active = :active)")
	List<User> findUserByRoleIdAndActiveAndClientId(long roleId, boolean active, long clientId);

	@Query("SELECT u FROM User u WHERE u.active = :active AND u.clientId = :clientId AND (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	List<User> getListUserByModuleCodeAndActiveAndClientId(List<String> code, Boolean active, long clientId);

	@Query("SELECT u.id FROM User u WHERE u.active = :active AND u.clientId = :clientId AND (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	List<Long> getListUserIdByModuleCodeAndActiveAndClientId(List<String> code, Boolean active, long clientId);

	@Query("SELECT count(*)>0 FROM User u WHERE u.id = :userId AND u.active = :active AND u.clientId = :clientId AND (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	boolean checkUserIdByModuleCodeAndActiveAndClientId(long userId, List<String> code, Boolean active, long clientId);

	@Query("SELECT u FROM User u WHERE u.active = :active AND u.clientId = :clientId AND u.org = :orgId AND (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	List<User> getListUserByModuleCodeAndOrgAndClientIdAndActive(List<String> code, long orgId, long clientId,
			boolean active);

	@Query("SELECT u.id FROM User u WHERE u.active = :active AND u.clientId = :clientId AND u.org = :orgId AND (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	List<Long> getListUserIdByModuleCodeAndOrgAndClientIdAndActive(List<String> code, long orgId, long clientId,
			boolean active);

	@Query("SELECT u.userName FROM User u WHERE u.active = :active AND u.clientId = :clientId AND u.org = :orgId AND (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	List<String> getListUserNameByModuleCodeAndOrgAndClientIdAndActive(List<String> code, long orgId, long clientId,
			boolean active);

	@Query("SELECT count(*)>0 FROM User u WHERE u.id = :userId AND u.active = :active AND u.clientId = :clientId AND u.org = :orgId AND (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	boolean checkUserIdByModuleCodeAndOrgAndActiveAndClientId(long userId, long orgId, List<String> code,
			boolean active, long clientId);

	@Query("SELECT u.org FROM User u WHERE u.userName = :userName AND u.active = :active AND u.clientId = :clientId")
	long findOrgIdByUserNameAndActiveAndClientId(String userName, boolean active, Long clientId);

	@Query("SELECT r FROM Role r WHERE r.active = :active AND r.clientId = :clientId AND ((:cabinet IS TRUE AND r.cabinet IS TRUE) OR (:cabinet IS NULL AND r.cabinet IS NULL)) AND (r.id IN "
			+ "(SELECT ur.roleId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.userId = :userId) "
			+ "OR r.id IN "
			+ "(SELECT pr.roleId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.posId = :posId))")
	List<Role> findRoleByUserIdAndActive(Long userId, Long posId, boolean active, long clientId, Boolean cabinet);

	@Query("SELECT r.id FROM Role r WHERE r.active = :active AND r.clientId = :clientId AND (r.id IN "
			+ "(SELECT ur.roleId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.userId = :userId) "
			+ "OR r.id IN "
			+ "(SELECT pr.roleId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.posId = :posId))")
	List<Long> findRoleIdByUserIdAndActive(Long userId, Long posId, boolean active, long clientId);

	@Query("Select u from User u join Category c on u.position = c.id where (lower(u.fullName) like %:textSearch% "
			+ " OR lower(u.userName) like %:textSearch% " + "OR lower(u.email) like %:textSearch%) "
			+ " AND u.org IN (:orgIds)" + " AND u.clientId =:clientId" + " AND (:active is null or u.active = :active)" + QUERY_ELSE
			+ " ORDER BY c.order ASC")
	List<User> findUserByOrgInAndClientIdAndActive(@Param(value = "orgIds") List<Long> orgIds,
			@Param(value = "textSearch") String textSearch, @Param(value = "clientId") Long clientId, @Param(value = "active") Boolean active);

	@Query("Select new com.vz.backend.core.dto.UserInfoDto(u.id, u.fullName, u.positionModel.name, u.orgModel.name) from User u join Category c on u.position = c.id where (lower(u.fullName) like %:textSearch% "
			+ " OR lower(u.userName) like %:textSearch% " + "OR lower(u.email) like %:textSearch%) "
			+ " AND u.org IN (:orgIds)" + " AND u.clientId =:clientId" + " AND (:active is null or u.active = :active)"
			+ " ORDER BY c.order ASC")
	Page<UserInfoDto> findUserByOrgInAndClientIdAndActive(@Param(value = "orgIds") List<Long> orgIds,
			@Param(value = "textSearch") String textSearch, @Param(value = "clientId") Long clientId, @Param(value = "active") Boolean active, Pageable pageable);

	@Query("SELECT u FROM User u WHERE u.active = true AND u.clientId = :clientId AND u.lead = true AND u.id NOT IN (:exceptUserList) "
			+ " AND u.org IN (SELECT u1.org FROM User u1 WHERE u1.active = true AND u1.clientId = :clientId AND u1.id IN (:userIds))")
	List<User> getLeadByListUserId(List<Long> userIds, List<Long> exceptUserList, Long clientId);

	@Query("SELECT u FROM User u WHERE (:active is null OR u.active = :active) AND (:orgId is null OR u.org = :orgId) AND (:posId is null OR u.position = :posId) AND u.clientId=:clientId")
	List<User> findListUserByOrgAndPositionIdAndActive(Long orgId, Long posId, Boolean active, Long clientId);

	@Query("SELECT u FROM User u WHERE (:active is null OR u.active = :active) AND (:orgId is null OR u.org = :orgId) AND u.position IN (:posId) AND u.clientId=:clientId")
	List<User> findListUserByOrgAndPositionIdInAndActive(Long orgId, Set<Long> posId, Boolean active, Long clientId);

	@Query("SELECT u FROM User u WHERE (:active is null OR u.active = :active) AND (u.org IN :orgIds) AND u.clientId = :#{#fromUser.clientId} "
			+ "AND u.position IN (SELECT df.toPositionId FROM DelegateFlow df "
			+ "WHERE df.active is true AND df.clientId = :#{#fromUser.clientId} AND df.fromPositionId = :#{#fromUser.position})")
	List<User> findListNguoiDuocUyQuyen(User fromUser, List<Long> orgIds, Boolean active);

	@Query("SELECT u FROM User u WHERE u.clientId = :clientId AND u.isLdap = :isLdap")
	List<User> findByClientIdAndLDAP(Long clientId, boolean isLdap);

	@Query("SELECT count(*)>0 FROM User u WHERE u.active is TRUE and u.photo=:fileName")
	boolean isAvatar(String fileName);

	@Query("SELECT u FROM User u WHERE u.org IN (:org) AND u.active = TRUE AND u.positionModel.isDefault = TRUE AND u.clientId = :clientId AND u.lead=TRUE")
	List<User> getListLeadByOrg(Long[] org, Long clientId);

	@Query("SELECT u FROM User u WHERE (:serialToken is null OR u.serialToken = :serialToken) AND u.clientId=:clientId")
	List<User> findUserBySerialNumber(String serialToken, Long clientId);

	User findBySerialTokenAndActiveTrue(String token);

	@Query("SELECT count(*)=0 FROM User u WHERE u.id IN :ids AND (u.clientId<>:clientId OR u.isLdap = TRUE)")
	boolean isSameClientId(List<Long> ids, Long clientId);

	@Modifying()
	@Query("UPDATE User u SET u.password = :newPassword WHERE (:ids is NULL or u.id IN :ids) and u.clientId=:clientId")
	void reset(List<Long> ids, String newPassword, Long clientId);

	@Query("SELECT count(*)>0 FROM User u LEFT JOIN ClericalOrg c ON u.id = c.userId AND c.active is true "
			+ "WHERE u.id = :userId AND u.active = :active AND u.clientId = :clientId "
			+ "AND (:orgId is null OR ((:clericalOrg is true AND c.orgId = :orgId) OR (:clericalOrg is false AND u.org = :orgId))) "
			+ "AND (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	boolean isVanThuByOrgIdAndCodeIn(boolean clericalOrg, long userId, Long orgId, List<String> code, boolean active, long clientId);

	@Query("SELECT u.id FROM User u LEFT JOIN ClericalOrg c ON u.id = c.userId AND c.active is true "
			+ "WHERE u.active = :active AND u.clientId = :clientId "
			+ "AND (:orgId is null OR ((:clericalOrg is true AND c.orgId = :orgId) OR (:clericalOrg is false AND u.org = :orgId))) "
			+ "AND (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	List<Long> getListVanThuByOrgIdAndCodeIn(boolean clericalOrg, List<String> code, Long orgId, boolean active, Long clientId);

	@Query("SELECT u.id FROM User u LEFT JOIN ClericalOrg c ON u.id = c.userId AND c.active is true "
			+ "WHERE u.active = :active AND u.clientId = :clientId AND u.lead = false "
			+ "AND (:orgId is null OR ((:clericalOrg is true AND c.orgId = :orgId) OR (:clericalOrg is false AND u.org = :orgId))) "
			+ "AND (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	List<Long> getListVanThuByOrgIdAndCodeInRemoveVT(boolean clericalOrg, List<String> code, Long orgId, boolean active, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.UserInfoDto(u.id, u.fullName, u.positionModel.name, u.orgModel.name) FROM User u "
			+ "WHERE u.clientId = :clientId AND (:active is null OR u.active = :active) "
			+ "AND (:orgId is null OR u.org = :orgId) AND (:posId is null OR u.position = :posId) "
			+ "AND (:fullName is null OR lower(u.fullName) like %:fullName%)")
	Page<UserInfoDto> findUserByOrgAndPositionAndFullNameAndActiveAndClientId(Long orgId, Long posId, String fullName, Boolean active, Long clientId,
			Pageable castToPageable);

	@Query("SELECT u.user.id FROM AuthorityUser u "
			+ "WHERE u.user.org = :orgId AND u.authority = :authority AND u.user.active = true AND u.active is true AND u.clientId = :clientId")
	List<Long> findUserIdByOrgWithAuthority(Long orgId, AuthorityEnum authority, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.UserDto(u.user.id, u.user.fullName) FROM AuthorityUser u "
			+ "WHERE u.user.org = :orgId AND u.authority = :authority AND u.user.active = true AND u.active is true AND u.clientId = :clientId")
	List<UserDto> findUserByOrgWithAuthority(Long orgId, AuthorityEnum authority, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.IdName(u.id, u.fullName) FROM User u "
			+ "WHERE u.id in :userIds AND u.clientId=:clientId")
	List<IdName> findByIds(Set<Long> userIds, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.ClericalWithOrgIds(u.id, u.fullName, u.positionModel.name, u.orgModel.name) "
			+ "FROM User u WHERE u.active = :active AND u.clientId = :clientId AND (:name is null OR lower(u.fullName) like %:name%) AND (:orgId is null OR u.org = :orgId) "
			+ "AND lower(u.positionModel.name) in ('văn thư văn phòng','văn thư đơn vị') and (u.id IN "
			+ "(SELECT ur.userId FROM UserRole ur WHERE ur.active = :active AND ur.clientId = :clientId AND ur.role.active = :active AND ur.role.clientId = :clientId AND ur.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))) "
			+ "OR u.position IN "
			+ "(SELECT pr.posId FROM PositionRole pr WHERE pr.active = :active AND pr.clientId = :clientId AND pr.role.active = :active AND pr.role.clientId = :clientId AND pr.roleId IN "
			+ "(SELECT p.roleId FROM Permission p WHERE p.active = :active AND p.clientId = :clientId AND p.module.active = :active AND p.module.code IN (:code))))")
	Page<ClericalWithOrgIds> getListUserInfoDtoByModuleCodeAndOrgIdAndActiveAndClientId(String name, List<String> code, Long orgId, Boolean active, Long clientId, Pageable page);

	@Query("SELECT u.id FROM User u WHERE u.org = :parentOrg AND u.positionModel.isLeadership = true AND u.active is true")
	List<Long> findAllLeadershipInParentOrg(Long parentOrg);

	@Query("SELECT u.phone FROM User u WHERE u.id = :userId")
	String getPhone(Long userId);

	@Query("SELECT new com.vz.backend.core.dto.UserBasicDto(u.id, u.fullName, u.positionModel.name) "
			+ "FROM User u WHERE u.org = :orgId AND u.positionModel.isLeadership = true AND u.active is true AND u.clientId = :clientId "
			+ "ORDER BY u.positionModel.order ASC")
	List<UserBasicDto> findAllLeadershipByOrgIdAndClientId(Long orgId, Long clientId);

	@Query("SELECT u.id FROM User u WHERE u.org IN (:orgIds) AND u.positionModel.isLeadership = true AND u.active is true AND u.clientId = :clientId")
	List<Long> findLeadershipByOrgIdIn(List<Long> orgIds, Long clientId);

	@Query("SELECT u.id FROM User u WHERE u.org IN (:orgIds) AND u.lead = true AND u.active is true AND u.clientId = :clientId")
	List<Long> findLeaderByOrgIdIn(List<Long> orgIds, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.UserFullNamePositionName(u.fullName, u.positionModel.name, u.email) FROM User u WHERE u.id =:userId")
	UserFullNamePositionName fullNamePositionName(Long userId);

	/* contact start */
	@Query("SELECT new com.vz.backend.core.dto.ContactDto(u.id, u.fullName, u.orgModel.name, u.phone, u.email, u.orgModel.id, u.orgModel.parentId) "
			+ "FROM User u where u.clientId = :clientId ")
	public Page<ContactDto> allContact(Long clientId, Pageable pageable);

	@Query("SELECT new com.vz.backend.core.dto.ContactDto(u.id, u.fullName, u.orgModel.name, u.phone, u.email, u.orgModel.id, u.orgModel.parentId) "
			+ "FROM User u where u.clientId = :clientId "
			+ "AND (:orgId is NULL OR u.org = :orgId) "
			+ "AND (:q is NULL OR lower(u.fullName) LIKE %:q% OR lower(u.phone) LIKE %:q% OR lower(u.email) LIKE %:q%)")
	public Page<ContactDto> searchContact(String q, Long orgId, Long clientId, Pageable pageable);

	@Query("SELECT new com.vz.backend.core.dto.ContactDto(u.id, u.fullName, u.orgModel.name, u.phone, u.email, u.orgModel.id, u.orgModel.parentId) "
			+ "FROM User u where u.clientId = :clientId "
			+ "AND (:orgId is NULL OR u.org = :orgId) "
			+ "AND (:q is NULL OR lower(u.fullName) LIKE %:q% OR lower(u.phone) LIKE %:q% OR lower(u.email) LIKE %:q%)")
	public List<ContactDto> searchContact2(String q, Long orgId, Long clientId, Sort sort);
	/* contact end */

	@Query("SELECT u.userName FROM User u WHERE u.userName IN :userNameList AND u.clientId = :ldapClientId")
	public List<String> exist(List<String> userNameList, Long ldapClientId);

	@Modifying
	@Query("UPDATE User u SET u.isLdap = true WHERE u.userName in :existUser AND u.clientId = :ldapClientId")
	void setLdap(List<String> existUser, Long ldapClientId);

	@Query("SELECT u.id FROM User u WHERE u.clientId =:clientId AND u.active = TRUE AND u.org IN :orgIds")
	List<Long> findByClientIdAndOrgInAndActiveTrue(Long clientId, List<Long> orgIds);

	@Query("SELECT new com.vz.backend.core.dto.UserInfoDto(u.user.id, u.user.fullName, u.user.positionModel.name, u.user.org, u.user.orgModel.name, u.user.lead) FROM AuthorityUser u "
			+ "WHERE u.user.org IN (:orgIds) AND (u.authority = :authority OR u.user.lead = TRUE) AND u.user.active = true AND u.active is true AND u.clientId = :clientId "
			+ "GROUP BY u.user.id, u.user.fullName, u.user.positionModel.name, u.user.org, u.user.orgModel.name, u.user.lead")
	List<UserInfoDto> findUserByListOrgWithAuthority(List<Long> orgIds, AuthorityEnum authority, Long clientId);

	@Query("SELECT new com.vz.backend.core.dto.UserInfoDto(u.id, u.fullName, u.positionModel.name, u.org, u.orgModel.name, u.lead) FROM User u "
			+ "WHERE u.org IN (:orgIds) AND u.active = true AND u.clientId = :clientId "
			+ "GROUP BY u.id, u.fullName, u.positionModel.name, u.org, u.orgModel.name, u.lead "
			+ "ORDER BY u.lead DESC")
	List<UserInfoDto> findUserByListOrgWithoutAuthority(List<Long> orgIds, Long clientId);

	@Query("Select new com.vz.backend.core.dto.UserInfoDto(u.id, u.fullName, u.positionModel.name, u.orgModel.name) from User u join Category c on u.position = c.id where (lower(u.fullName) like %:textSearch% "
			+ " OR lower(u.userName) like %:textSearch% " + "OR lower(u.email) like %:textSearch%) "
			+ " AND u.org IN (:orgIds)" + " AND u.clientId =:clientId" + " AND (:active is null or u.active = :active)"
			+ " ORDER BY u.fullName ASC")
	List<UserInfoDto> findUserByOrgInAndClientIdAndActives(@Param(value = "orgIds") List<Long> orgIds,
			@Param(value = "textSearch") String textSearch, @Param(value = "clientId") Long clientId, Boolean active);

	@Query("SELECT new com.vz.backend.core.dto.SearchNameDto(u.fullName, u.positionModel.name) FROM User u "
			+ "WHERE (lower(u.fullName) like %:name% OR lower(u.email) like %:name%) "
			+ "AND u.clientId =:clientId" + " AND u.active=TRUE "
			+ "ORDER BY u.fullName ASC")
	List<SearchNameDto> searchName(String name, Long clientId);

	@Query("SELECT NEW com.vz.backend.core.dto.LabelValueId(u.id, u.cert, u.fullName, u.orgModel.name) FROM User u WHERE u.clientId =:clientId AND u.active = TRUE AND u.id IN (:userIds) AND (:cert IS NULL OR u.cert IS NOT NULL)")
	List<LabelValueId<String>> getCertAndFullNameByUserId(List<Long> userIds, Boolean cert, Long clientId);

	@Query("SELECT NEW com.vz.backend.core.dto.LabelValueId(u.id, u.cert, u.fullName, u.orgModel.name) FROM User u WHERE u.clientId =:clientId AND u.active = TRUE AND u.id IN (:userIds) AND u.lead = TRUE AND (:cert IS NULL OR u.cert IS NOT NULL)")
	List<LabelValueId<String>> getCertAndFullNameByUserIdManager(List<Long> userIds, Boolean cert, Long clientId);

	@Query("SELECT u.id FROM User u WHERE u.fullName IN (:fullNames) AND u.clientId = :clientId AND u.active = TRUE")
	List<Long> findUserIdByFullName(String[] fullNames, Long clientId);

	@Query("Select new com.vz.backend.core.dto.UserBasicDto(u) "
			+ " FROM User u join Category c on u.position = c.id "
			+ " WHERE u.org = (:orgId) AND u.clientId =:clientId AND u.active = TRUE AND (:isLead IS NULL OR u.lead IS TRUE) AND u.fullName is not null "
			+ " ORDER BY c.order, u.fullName ASC")
	List<UserBasicDto> findBasicInfoByOrgId(Long orgId, Boolean isLead, Long clientId);

	@Query("SELECT u.id FROM User u WHERE u.org IN (:orgs) AND u.active = TRUE AND u.clientId = :clientId AND (u.positionModel.isDefault = TRUE OR u.lead = TRUE)")
	List<Long> getLeadBasicByOrgs(Long[] orgs, Long clientId);
	
	User findFirstByEmailAndActiveTrue(String email);

	User findFirstBySerialToken(String serialToken);

	User findFirstByUserName(String userName);

	@Query("SELECT u.id FROM User u WHERE u.org IN (:orgIds) AND u.active = TRUE AND u.clientId = :clientId ")
	List<Long> findUserIdByOrgIds(List<Long> orgIds, Long clientId);

	@Query("SELECT COUNT(1) FROM User u WHERE u.global = TRUE AND u.active IS TRUE AND u.id IN (:userIds) AND u.clientId = :clientId")
	long validUserIdsByOutside(List<Long> userIds, Long clientId);

	@Query("SELECT u.id FROM User u WHERE u.fullName IN (:fullNames) AND u.active = TRUE AND u.clientId = :clientId ")
	List<Long> getIdsByFullNames(List<String> fullNames, Long clientId);

	@Query("SELECT gu.userId FROM Group g INNER JOIN GroupUser gu on g.id=gu.groupId and g.id =:id AND g.clientId = :clientId ")
	List<Long> getUserIdByGroupId(Long id, Long clientId);

	@Query("SELECT u.id FROM User u WHERE u.clientId =:clientId AND u.active = TRUE AND u.id IN (:userIds) AND (:cert IS NULL OR u.cert IS NOT NULL)")
	List<Long> getCertByUserId(List<Long> userIds, Boolean cert, Long clientId);

	@Query("SELECT count(1) > 0 FROM User u WHERE lower(u.fullName) =:fullName AND u.active = TRUE AND u.clientId = :clientId ")
	boolean getUserByFullName(String fullName, Long clientId);

	@Query("SELECT u FROM User u where u.active is true "
			+ "AND ((:org) is null OR u.org IN ( :org)) "
			+ "AND (:position is null OR u.position = :position) "
			+ "AND (:clientId is null or u.clientId = :clientId) ")
	Page<User> findUserByPositionAndOrg(@Param(value = "org") List<Long> org, @Param(value = "position") Long position,
						@Param(value = "clientId") Long clientId, Pageable pageable);

	@Query("SELECT u FROM User u WHERE u.clientId=:clientId AND u.active=true and u.org=:org AND u.positionModel.isLeadership = true "
			+ " order by u.positionModel.order ASC")
	List<User> getAllUserbyOrg(Long clientId, Long org);

	@Query("select u.id from Category c inner join User u on u.position = c.id where lower(c.name) = :name and u.org = :orgBanId and u.active is true")
	Long getUserVanThuBan(String name, Long orgBanId);

	@Query("select u.id from Category c inner join User u on u.position = c.id where lower(c.name) = :name and u.org = :orgBanId and u.active is true")
	List<Long> getUsersByNameCategory(String name, Long orgBanId);

	@Query("select u.id from Category c inner join User u on u.position = c.id where lower(c.name) = :name and u.org in(:orgBanId) and u.active is true")
	Long getUserVanThuBan(String name, List<Long> orgBanId);

	@Query("select u.id from Category c inner join User u on u.position = c.id where lower(c.name) = :name and u.org in(:orgBanId) and u.active is true")
	List<Long> getUsersByNameCategory(String name, List<Long> orgBanId);

	@Query("select u.id from Category c inner join User u on u.position = c.id where lower(c.name) in(:name) and u.org = :orgBanId and u.active is true") 
	List<Long> getUserLeadOrgBan(List<String> name, Long orgBanId);

	@Query("select u.id from Category c inner join User u on u.position = c.id where lower(c.name) in(:name) and u.org in(:orgBanId) and u.active is true")
	List<Long> getUserByPositionAndOrg(List<String> name, List<Long> orgBanId);

	@Query("select u from User u where u.clientId=:clientId and u.cert=:cert")
	List<User> findByCertAndClientId(Long clientId, String cert);

	@Query("select u from User u where u.clientId=:clientId and u.serialToken=:serialToken")
	List<User> findByTokenAndClientId(Long clientId, String serialToken);

	@Query("SELECT new com.vz.backend.core.dto.UserInfoDto(u.id, u.fullName, u.positionModel.name, u.orgModel.name, u.positionModel.order) FROM User u "
			+ "WHERE u.clientId = :clientId AND (:active is null OR u.active = :active) "
			+ "AND u.org in :orgId AND (:posId is null OR u.position = :posId) "
			+ "AND (:fullName is null OR lower(u.fullName) like %:fullName%) "
			+ "ORDER BY u.orgModel.level ASC, u.orgModel.order ASC, u.positionModel.order ASC ")
	Page<UserInfoDto> findUserByOrgAndPositionUsingGroupUser(List<Long> orgId, Long posId, String fullName, Boolean active, Long clientId,
																			  Pageable castToPageable);

	@Query("select count(1)>0 from GroupUser u where u.clientId=:clientId and u.active=:active and u.userId=:userId and u.groupId in(:groupIds)")
	Boolean checkUserInListGroup(Long clientId, Boolean active, Long userId,List<Long> groupIds);

	@Query("select u from User u where u.clientId=:clientId and u.active=true and u.position=:positionId and u.orgModel.parentId=:orgId")
	List<User> getListUserByPositionIdWithParentOrg(Long clientId, Long orgId, Long positionId);

	@Query("SELECT NEW com.vz.backend.core.dto.UserBasicDto(u.id, u.fullName, u.orgModel.name, u.positionModel.name, u.org) FROM User u "
			+ "WHERE (:text IS NULL OR LOWER(u.fullName) LIKE %:text% OR LOWER(u.email) LIKE %:text% OR LOWER(u.positionModel.name) LIKE %:text%) "
			+ "AND u.clientId = :clientId AND u.active=TRUE AND (:orgId IS NULL OR u.org =:orgId) " + QUERY_ELSE
			+ "ORDER BY u.positionModel.order ASC ")
	Page<UserBasicDto> search(Long orgId, String text, Long clientId, Pageable pageable);
}
