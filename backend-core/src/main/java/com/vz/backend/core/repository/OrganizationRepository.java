package com.vz.backend.core.repository;

import com.vz.backend.core.domain.OrgConfigSign;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Repository
public interface OrganizationRepository extends IRepository<Organization> {

    @Query(value = "SELECT c FROM Organization c WHERE :code is null or c.code = :code")
    Organization findByCode(@Param(value = "code") Long code);

    @Query(value = "SELECT c FROM Organization c WHERE (:name is null or c.name = :name) and (:clientId is null or c.clientId = :clientId) and (:parentId is null or c.parentId = :parentId)")
    Organization findByNameAndClientIdAndParentId(String name, Long clientId, Long parentId);

    @Query("SELECT org FROM Organization org "
            + "WHERE org.clientId=:clientId AND ((:parentId is NULL AND org.parentId is NULL) OR org.parentId=:parentId) AND (:active is null or org.active=:active)")
    List<Organization> findByClientIdAndParentIdAndActive(@Param(value = "clientId") Long clientId,
                                                          @Param(value = "parentId") Long parentId, @Param(value = "active") Boolean active, Sort sort);

    @Query("SELECT org.id FROM Organization org "
            + "WHERE org.clientId = :clientId AND ((:parentId is NULL AND org.parentId is NULL) OR org.parentId=:parentId) AND (:active is null or org.active=:active) ORDER BY org.order ASC")
    List<Long> findByClientIdAndParentIdAndActive(Long parentId, Long clientId, Boolean active);

    @Query("SELECT org.parentId FROM Organization org WHERE org.id = :orgId AND org.clientId = :clientId")
    Long findParentIdByOrgIdAndClientId(Long orgId, Long clientId);

    @Query("SELECT new com.vz.backend.core.dto.OrganizationBasicDto(org.parentOrg.id, org.parentOrg.name) FROM Organization org WHERE org.id = :orgId AND org.clientId = :clientId")
    OrganizationBasicDto findParentByOrgIdAndClientId(Long orgId, Long clientId);

    @Query("SELECT new com.vz.backend.core.dto.OrganizationBasicDto(org.id, org.name) FROM Organization org WHERE org.id = :orgId AND org.clientId = :clientId")
    OrganizationBasicDto findByOrgIdAndClientId(Long orgId, Long clientId);

    @Query("SELECT p.parentId FROM Organization p WHERE p.id=:orgId and p.clientId = :clientId")
    Long getParentId(@Param("orgId") Long orgId, @Param(value = "clientId") Long clientId);

    @Query("SELECT p FROM Organization p WHERE p.id=:orgId and p.clientId = :clientId")
    Organization getParentIdObj(@Param("orgId") Long orgId, @Param(value = "clientId") Long clientId);

    @Query("select o from Organization o where ((:orgIds) is null or o.id in (:orgIds)) and o.clientId = :clientId and o.active = :active ")
    List<Organization> getOrgByIdList(Long clientId, List<Long> orgIds, Boolean active);

    @Query("select o.id from Organization o where o.clientId = :clientId and o.active = :active")
    List<Long> getIdList(Long clientId, Boolean active);

    @Query("SELECT count(*)>0 FROM Organization o INNER JOIN User u ON o.id = :orgId AND o.id = u.org AND u.active is true AND u.clientId=:clientId")
    boolean existUserByOrgId(Long orgId, Long clientId);

    @Query("SELECT count(*)>0 FROM Organization o INNER JOIN Organization c ON o.id = :orgId AND o.id = c.parentId AND c.active is true AND c.clientId=:clientId ")
    boolean existChildByOrgId(Long orgId, Long clientId);

    @Query("SELECT count(*)>0 FROM Organization o LEFT JOIN Organization p ON o.parentId=p.id "
            + "WHERE o.id=:orgId AND p.active is FALSE AND o.clientId=:clientId AND p.clientId=:clientId")
    boolean hasInActiveParent(Long orgId, Long clientId);

    boolean existsByIdAndActiveAndClientId(Long id, boolean active, Long clientId);

    @Query("SELECT new com.vz.backend.core.dto.OrgParent(o.parentId, o.id, o.name, o.order) FROM Organization o WHERE o.clientId = :clientId AND (:name is null or lower(o.name) like %:name%) AND o.active IS TRUE ORDER BY o.order ASC")
    List<OrgParent> getParentChild(@Param("clientId") Long clientId, String name);

    @Query("SELECT p FROM Organization p WHERE p.name = :name and p.active = true and p.clientId = :clientId and (p.parentId =:parentId)")
    List<Organization> findByNameAndParentId(String name, Long parentId, Long clientId);

    @Query("SELECT p FROM Organization p WHERE p.phone = :phone and p.active = true and p.clientId = :clientId and (p.parentId is null or p.parentId =:parentId)")
    List<Organization> findByPhoneAndParentId(String phone, Long parentId, Long clientId);

    @Query("SELECT p FROM Organization p WHERE " + " (:#{#dto.phone} is null or p.phone like %:#{#dto.phone}%) "
            + " and (:#{#dto.name} is null or lower(p.name) like %:#{#dto.name}%) "
            + " and (:#{#dto.address} is null or lower(p.address) like %:#{#dto.address}%) "
            + " and (:#{#dto.email} is null or lower(p.email) like %:#{#dto.email}%) "
            + " and (:#{#dto.parentId} is null or p.parentId =:#{#dto.parentId} or p.id =:#{#dto.parentId}) "
            + " and (:#{#dto.orgType} is null or p.orgType =:#{#dto.orgType}) "
            + " and (:#{#dto.id} is null or p.id =:#{#dto.id}) "
            + " and (:#{#dto.active} is null or p.active =:#{#dto.active}) " + " and p.clientId = :clientId "
            + " order by  p.level ASC, p.order ASC")
    Page<Organization> search(OrgDto dto, Long clientId, Pageable castToPageable);

    @Query("SELECT p FROM Organization p WHERE p.clientId =:clientId and p.isLdap = :isLdap")
    List<Organization> findByClientIdAndLDAP(long clientId, boolean isLdap);

    @Query("SELECT p FROM Organization p WHERE p.clientId =:clientId and p.active=true " +
            " and (p.parentId is null or p.parentId in (select org.id FROM Organization org WHERE org.clientId =:clientId and org.active=true and org.parentId is null ))")
    List<Organization> findByParentIdAndActiveAAndClientId(long clientId);

    @Query("SELECT p FROM Organization p WHERE p.clientId =:clientId and p.orgType = :orgType and p.active=:active and p.parentId = :parentId")
    List<Organization> getChildrenByBpmnIdAndParentId(Long clientId, Boolean active, Long parentId, Long orgType);

    Organization findByClientIdAndActiveAndParentId(Long clientId, Boolean active, Long parentId);

    @Query("SELECT new com.vz.backend.core.dto.ConfigSignDto(c.orgId, c.place, c.tl, c.user.fullName, c.user.signature, c.user.positionModel.name) "
            + "FROM OrgConfigSign c WHERE c.clientId =:clientId and c.orgId in :parentIds")
    List<ConfigSignDto> findConfigByOrgIds(List<Long> parentIds, Long clientId);

    @Query("SELECT c FROM OrgConfigSign c WHERE c.orgId=:orgId")
    OrgConfigSign findOrgSign(Long orgId);

    @Modifying
    @Query("DELETE from OrgConfigSign c where c.orgId=:orgId")
    void deleteOrgSign(Long orgId);

    @Query("SELECT org.id FROM Organization org "
            + "WHERE (((:orgId is NULL AND org.parentId is NULL) OR org.parentId=:orgId) "
            + "OR (org.id = (SELECT org1.parentId FROM Organization org1 WHERE org1.id = :orgId)) "
            + "OR (org.parentId = (SELECT org2.parentId FROM Organization org2 WHERE org2.id = :orgId))) "
            + "AND org.clientId=:clientId AND (:active is null or org.active=:active)")
    List<Long> findParentAndSubAndSameOrgByCurrOrg(Long clientId, Long orgId, boolean active);

    @Query("select new com.vz.backend.core.dto.OrgParent(o.parentId, o.id, o.name) from Organization o where o.id in (:orgIds) ORDER BY o.order ASC")
    List<OrgParent> getOrgByListId(List<Long> orgIds);

    @Query("SELECT distinct u.org FROM User u WHERE u.positionModel.isLeadership = true AND u.active is true AND u.clientId = :clientId")
    List<Long> findOrgHasLeaderShip(Long clientId);

    Organization findByIdentifierAndClientIdAndActiveTrue(String identifier, Long clientId);

    List<Organization> findByClientIdAndParentIdAndActiveTrue(Long clientId, Long parentId);

    @Query("SELECT new com.vz.backend.core.dto.OrgGroupDto(o.name, o.id) FROM Organization o "
            + "WHERE lower(o.name) like %:name%  "
            + "AND o.clientId =:clientId" + " AND o.active=TRUE "
            + "ORDER BY o.name ASC")
    List<OrgGroupDto> searchName(String name, Long clientId);

    @Query("SELECT NEW com.vz.backend.core.dto.OrgGroupDto(o.name, o.id) FROM Organization o WHERE o.clientId = :clientId AND o.id IN :orgIds AND o.active IS TRUE")
    List<OrgGroupDto> findByClientIdAndIdInAndActiveTrue(Long clientId, List<Long> orgIds);

    @Query("SELECT NEW com.vz.backend.core.dto.IdName(o.id, o.name) FROM Organization o WHERE o.global = TRUE AND o.active is true")
    List<IdName> getAllByGlobal();

    @Query("SELECT COUNT(1) FROM Organization o WHERE o.global = TRUE AND o.active IS TRUE AND o.id IN (:orgIds) AND o.clientId = :clientId")
    long validOrgIdsByOutside(List<Long> orgIds, Long clientId);

    @Query("SELECT p FROM Organization p WHERE lower(p.name) LIKE %:name% and p.active = true and p.clientId = :clientId ")
    List<Organization> getOrgByName(String name, Long clientId);

    @Query("SELECT p FROM Organization p WHERE p.clientId =:clientId and (:parentId is null or p.parentId = :parentId) and p.active is true")
    List<Organization> getOrganizationByParentId(@Param(value = "clientId") Long clientId, @Param(value = "parentId") Long parentId);

    @Query("SELECT p FROM Organization p inner join User u on p.id = u.orgModel.id and p.clientId =:clientId  and u.id =:userId")
    Organization getOrganizationByUserCreate(@Param(value = "clientId") Long clientId, @Param(value = "userId") Long userId);

    Organization findByIdentifierAndActiveTrue(String identifier);

    @Query("SELECT org FROM Organization org where org.clientId =:clientId  and org.active =:active and org.id = :id")
    Organization findByClientIdAndActiveAndId(Long clientId, Boolean active, Long id);

    @Query("select count(u) from User u join Organization o on u.org = o.id where o.id = :orgId and u.clientId = :clientId and u.active = :active")
    Long countUsers(Long orgId, Long clientId, Boolean active);
    
    @Query("SELECT org FROM Organization org where org.clientId =:clientId  and org.active =:active and ((:parentId is null and org.parentId is null) or org.parentId = :parentId)")
    List<Organization> getAllByParentId(Long clientId, Boolean active, Long parentId);


    @Query("select u from User u join Organization o on u.org = o.id where o.id = :orgId and u.clientId = :clientId and o.clientId = :clientId and o.active = true and u.lead = true and u.active = :active")
    List<User> getLeaders(Long orgId, Long clientId, Boolean active);
}
