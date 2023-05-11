package com.vz.backend.core.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;

import com.vz.backend.core.domain.GroupUser;
import com.vz.backend.core.dto.UserInfoDto;

public interface IGroupUserRepository extends IRepository<GroupUser> {

    List<GroupUser> findByGroupIdAndClientId(Long groupId, Long clientId);

    String QUERY_USER_IN_GROUP = "SELECT new com.vz.backend.core.dto.UserInfoDto(u.id, u.fullName, u.positionModel.name, u.orgModel.name) FROM User u INNER JOIN GroupUser g ON u.id = g.userId "
            + "WHERE g.active is true AND g.groupId = :groupId AND g.clientId = :clientId AND (:name is null OR lower(u.fullName) like %:name%) group by u.id, u.fullName, u.positionModel.name, u.positionModel.order, u.orgModel.name order by u.positionModel.order ASC";

    @Query(QUERY_USER_IN_GROUP)
    List<UserInfoDto> findUserInfoByGroupIdAndClientId(Long groupId, Long clientId, String name);

    @Query(QUERY_USER_IN_GROUP)
    Page<UserInfoDto> pageUserInGroup(Long groupId, Long clientId, String name, Pageable pageable);

    List<GroupUser> findByClientIdAndGroupIdAndActiveTrue(Long clientId, Long groupId);

}
