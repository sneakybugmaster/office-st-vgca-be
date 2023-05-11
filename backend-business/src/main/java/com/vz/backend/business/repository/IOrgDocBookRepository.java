package com.vz.backend.business.repository;

import java.util.List;
import java.util.Set;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.OrgDocBook;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IOrgDocBookRepository extends IRepository<OrgDocBook> {

	List<OrgDocBook> findByBookIdAndClientId(Long bookId, Long clientId);

	@Query("SELECT DISTINCT db.orgId FROM OrgDocBook db WHERE db.bookId = :bookId AND db.clientId = :clientId AND db.active = :active")
	Set<Long> findOrgIdByBookIdAndClientIdAndActive(Long bookId, Long clientId, boolean active);

	@Query("SELECT odb.orgId FROM OrgDocBook odb WHERE odb.bookId = :bookId AND odb.active = :active")
	List<Long> findOrgIdByBookIdAndActive(Long bookId, boolean active);

	
	@Modifying
	void deleteByBookIdAndOrgIdIn(Long bookId, Set<Long> deleteIds);

}
