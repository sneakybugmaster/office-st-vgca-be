package com.vz.backend.core.repository;

import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.domain.UserCategory;
import com.vz.backend.core.dto.CategoryDto;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;
import java.util.Optional;

@Repository
public interface IUserCategoryRepository extends IRepository<UserCategory> {

    @Query(value = "SELECT uc FROM UserCategory uc WHERE uc.category.id=:categoryId and uc.user.active=true and uc.clientId=:clientId")
    List<UserCategory> findByCategoryId(long categoryId, Long clientId);

    @Query(value = "SELECT uc.user FROM UserCategory uc WHERE uc.category.id=:categoryId and uc.user.active=true and uc.active=:active and uc.clientId=:clientId order by uc.user.fullName")
    List<User> findByCategoryIdAndActive(@Param(value = "categoryId") long categoryId, @Param(value = "active") boolean active,
                                         @Param(value = "clientId") Long clientId);

    @Query(value = "SELECT DISTINCT uc.category FROM UserCategory uc inner join UserOrganization uo on (uc.userOrganizationId = uo.id and uc.userId = uo.userId) WHERE uc.userId=:userId and (:orgId is null or uo.orgId=:orgId) and uc.user.active=true and uc.active=:active and uc.category.active=true and uc.clientId=:clientId order by uc.category.name")
    List<Category> findCategoryByUserIdAndOrgIdAndActive(@Param(value = "userId") Long userId, @Param(value = "active") boolean active,
                                                         @Param(value = "clientId") Long clientId, @Param(value = "orgId") Long orgId);

    @Query(value = "SELECT new com.vz.backend.core.dto.CategoryDto(cate.id, org.id, cate.name, org.name) " +
            "FROM Category cate inner join UserCategory uc on (cate.id = uc.categoryId) inner join UserOrganization uo on (uc.userOrganizationId = uo.id and uc.userId = uo.userId)  inner join Organization org on (uo.orgId = org.id) " +
            "WHERE uc.userId=:userId and uc.clientId=:clientId order by uc.category.name")
    List<CategoryDto> findCategoryByUserIdAndActive(@Param(value = "userId") Long userId,
                                                            @Param(value = "clientId") Long clientId);

    @Query(value = "SELECT DISTINCT uc.categoryId FROM UserCategory uc WHERE uc.userId=:userId and uc.user.active=true and uc.active=:active and uc.category.active=true and uc.clientId=:clientId")
    List<Long> findCategoryIdByUserIdAndActive(@Param(value = "userId") Long userId,
                                               @Param(value = "active") boolean active, @Param(value = "clientId") Long clientId);

    @Query(value = "SELECT uc FROM UserCategory uc WHERE uc.category.id=:categoryId and uc.user.id=:userId and uc.active=:active and uc.clientId =:clientId order by uc.user.fullName")
    UserCategory findByCategoryIdAndUserIdAndActive(@Param(value = "categoryId") long categoryId, @Param(value = "userId") long userId,
                                                    @Param(value = "active") boolean active, @Param(value = "clientId") Long clientId);

    @Query(value = "SELECT uc.category FROM UserCategory uc WHERE uc.user.userName=:userName and uc.clientId=:clientId")
    List<Category> findCategoryByUserName(@Param(value = "userName") String userName, @Param(value = "clientId") Long clientId);

    @Query(value = "SELECT uc FROM UserCategory uc WHERE uc.userId in (:userIds) and uc.categoryId =:categoryId and uc.clientId =:clientId and uc.active = true")
    List<UserCategory> findCategoryByListUserId(List<Long> userIds, Long categoryId, Long clientId);

    @Query(value = "SELECT uc FROM UserCategory uc WHERE uc.userId = :userId  and uc.clientId =:clientId and uc.active = true")
    List<UserCategory> findCategoryByUserId(Long clientId, Long userId);

    Optional<UserCategory> findByUserIdAndCategoryId(Long userId, Long categoryId);
}
