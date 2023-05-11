package com.vz.backend.core.repository;

import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.UserOrganization;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IUserOrganizationRepository extends IRepository<UserOrganization> {

    @Query(value = "SELECT org FROM UserOrganization uo inner join Organization org on (uo.orgId = org.id) where uo.userId = :userId and org.active = :active and org.clientId = :clientId")
    List<Organization> getAllOrgByUserId(Long clientId, Long userId, Boolean active);

    List<UserOrganization> findByClientIdAndUserIdAndActiveTrue(Long clientId, Long userId);
}
