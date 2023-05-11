package com.vz.backend.core.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.core.domain.CategoryType;

@Repository
public interface ICateroryTypeRepository extends IRepository<CategoryType> {
	CategoryType findByClientIdAndName(Long clientId, String name);

	CategoryType findByClientIdAndCode(Long clientId, String categoryTypeCode);

	@Query("select t from CategoryType t where t.clientId = :clientId")
	Page<CategoryType> findClientId(Long clientId, Pageable page);

	@Query("select t from CategoryType t where t.clientId = :clientId and t.code in (:codes) and t.active = :active")
	List<CategoryType> findByClientIdAndCodeAndActive(Long clientId, String[] codes, boolean active);
}