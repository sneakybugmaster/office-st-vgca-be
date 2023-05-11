package com.vz.backend.business.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.CategoryDocBook;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ICategoryDocBookRepository extends IRepository<CategoryDocBook> {

	List<CategoryDocBook> findByBookIdAndClientId(Long bookId, Long clientId);

	@Query("SELECT DISTINCT db.categoryId FROM CategoryDocBook db WHERE db.bookId = :bookId AND db.clientId = :clientId AND db.active = :active")
	Set<Long> findOrgIdByBookIdAndClientIdAndActive(Long bookId, Long clientId, boolean active);

	@Query("SELECT cdb.categoryId FROM CategoryDocBook cdb WHERE cdb.bookId = :bookId AND cdb.clientId = :clientId AND cdb.active = :active")
	List<Long> findCategoryIdByBookIdAndActive(Long bookId, Long clientId, boolean active);
}
