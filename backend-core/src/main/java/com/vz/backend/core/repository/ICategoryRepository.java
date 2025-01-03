package com.vz.backend.core.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.core.domain.Category;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.dto.LabelValueId;

@Repository
public interface ICategoryRepository extends IRepository<Category>, ICategoryRepositoryCustom {

	@Query("SELECT c FROM Category c WHERE c.clientId=:clientId AND c.name=:name AND c.categoryType.code=:code")
	Category findByClientIdAndNameAndCode(Long clientId, String name, String code);

	Category findByClientIdAndName(Long clientId, String name);

	Category findBySyncCodeAndClientId(Long syncCode, Long clientId);

	List<Category> findByClientIdAndCategoryTypeId(Long clientId, long categoryTypeId, Sort sort);

	@Query("select c from Category c where c.categoryTypeId in (:categoryTypeIds) and c.clientId = :clientId and c.active = :active ORDER BY order")
	List<Category> findByClientIdAndCategoryTypeIdAndActive(Long clientId, Long[] categoryTypeIds, boolean active);

	List<Category> findByClientIdAndCategoryTypeIdAndActive(Long clientId, long categoryTypeId, boolean active,
			Sort sort);

	@Query(value = "SELECT c FROM Category c WHERE" + " (:name IS NULL OR LOWER(c.name) LIKE %:name%)"
			+ " AND (:id is null or c.id = :id)"
			+ " AND (:categoryTypeId is null or c.categoryTypeId = :categoryTypeId)"
			+ " AND (:clientId is null or c.clientId = :clientId)"
			+ " AND (coalesce(:active, null) IS NULL or c.active = :active)")
	Page<Category> findByClientIdAndCategoryTypeId(String name, Boolean active, Long id, Long categoryTypeId,
			Long clientId, Pageable page);

	@Query("select max(c.order) from Category c where c.clientId = :clientId and c.categoryTypeId = :categoryTypeId")
	Long getMaxOrderByCategoryType(Long clientId, long categoryTypeId);

	@Query(value = "select c from Category c join CategoryType t on c.categoryTypeId = t.id "
			+ "where c.clientId = :clientId and t.clientId =:clientId and t.code = :code and t.active = true and c.active = true ORDER BY c.order DESC")
	List<Category> findByCategoryCodeAndClientId(String code, Long clientId);
	
	@Query(value = "select new com.vz.backend.core.dto.LabelValueId(c.id, c.code, c.name) from Category c join CategoryType t on c.categoryTypeId = t.id "
			+ "where c.clientId = :clientId and t.clientId =:clientId and t.code = :code and t.active = true and c.active = true ORDER BY c.order DESC")
	List<LabelValueId<String>> findByCategoryCode(String code, Long clientId);

	@Query("select c from Category c where ((:type is null) or c.categoryTypeId = (:type)) and c.clientId = :clientId and c.name = :name")
	List<Category> findByClientIdAndNameAndCategoryTypeId(Long clientId, String name, Long type);

	@Query("select c from Category c where c.categoryTypeId in (:categoryType) and c.clientId = :clientId and c.isLdap = :isLdap")
	List<Category> findByClientIdAndCategoryTypeIdAndLDAP(long clientId, long categoryType, boolean isLdap);

	@Query("SELECT new com.vz.backend.core.dto.IdName(p.id, p.name) FROM Category p "
			+ "WHERE p.id in :positionIds AND p.clientId=:clientId")
	List<IdName> findByIds(Set<Long> positionIds, Long clientId);

	@Query("SELECT count(*) > 0 FROM Category c WHERE c.id = :id AND c.isLeadership = true")
	boolean isLeadership(Long id);
	
	@Query("SELECT c FROM Category c WHERE c.clientId = :clientId AND c.categoryType.code = :catDocType")
	List<Category> findByCodeAndClientId(String catDocType, Long clientId);

	@Query("SELECT c FROM Category c WHERE c.clientId = :clientId AND c.categoryType.code = :code AND c.name = :name AND c.active = TRUE")
	Category findByClientIdAndNameAndCategoryCode(String name, String code, Long clientId);

	@Query("SELECT c FROM Category c WHERE lower(:name) = lower(c.name) and c.categoryType.code = :code and c.clientId=:clientId and c.active IS TRUE")
	List<Category> findByNameAndCodeAndClientIdCaseInsensitive(String name, String code, Long clientId);

	@Query(value = "select count(1) > 0 from Category c join CategoryType t on c.categoryTypeId = t.id "
			+ "where c.clientId = :clientId and t.clientId =:clientId and t.code = :code and t.active = true and c.active = true and c.id=:id")
	Boolean checkPositionExits(String code, Long clientId, Long id);

	@Query("SELECT c FROM Category c WHERE c.name=:name AND c.categoryType.code=:code")
	Category findByNameAndCode(String name, String code);

	@Query(value = "SELECT c FROM Category c WHERE" + " c.clientId=:clientId AND (:name IS NULL OR LOWER(c.name) LIKE %:name%)")
	Category findByName(Long clientId, String name);
}
