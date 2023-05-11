package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.vz.backend.business.domain.DocumentBook;
import com.vz.backend.business.dto.DocumentBookDetailDto;
import com.vz.backend.business.dto.DocumentBookDto;
import com.vz.backend.core.repository.IRepository;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Repository
public interface IDocumentBookRepository extends IRepository<DocumentBook> {

	List<DocumentBook> findByActiveAndClientId(Boolean active, Long clientId);

	@Query(value = "SELECT db FROM DocumentBook db WHERE ((:typeId is null OR db.bookType = :typeId)) and db.active=:active and db.clientId=:clientId")
	List<DocumentBook> findByActiveAndBookType(Long typeId, Boolean active, Long clientId);

	@Query(value = "SELECT db FROM DocumentBook db WHERE db.id IN"
			+ "(SELECT odb.bookId FROM OrgDocBook odb WHERE odb.orgId IN :orgIds AND odb.active = true) "
			+ "AND ((:typeId is null OR db.bookType = :typeId)) and db.active=:active and db.clientId=:clientId "
			+ "ORDER BY db.name")
	List<DocumentBook> findByOrgIdAndBookTypeAndActiveAndClientId(List<Long> orgIds, Long typeId,
			Boolean active, Long clientId);

	@Query(value = "SELECT new com.vz.backend.business.dto.DocumentBookDto(db.id, db.active, db.name, db.startNumber, db.currentNumber, db.bookType, db.numberOrSign, db.year, db.org.id, db.org.name) FROM DocumentBook db WHERE 1=1 and (:type is null or db.bookType = :type) and (:name is null or lower(db.name) like %:name%) AND (:year is null OR db.year = :year) and (:active is null or db.active=:active) and db.clientId=:clientId")
	Page<DocumentBookDto> searchDocumentBook(String name, Long type, Integer year, Boolean active, Long clientId,
			Pageable pageable);

	@Query(value = "SELECT new com.vz.backend.business.dto.DocumentBookDto(db.id, db.active, db.name, db.startNumber, db.currentNumber, db.bookType, db.numberOrSign, db.year, db.org.id, db.org.name) "
			+ "FROM DocumentBook db "
			+ "WHERE 1=1 AND (db.orgCreateId = :orgId OR (SELECT count(*)>0 FROM OrgDocBook odb WHERE db.id = odb.bookId AND (:active is null or odb.active=:active) AND odb.orgId = :orgId) is TRUE) "
			+ "AND (:type is null or db.bookType = :type) and (:name is null or lower(db.name) like %:name%) "
			+ "AND (:year is null OR db.year = :year) and (:active is null or db.active=:active) and db.clientId=:clientId")
	Page<DocumentBookDto> searchDocumentBookWithOrgId(Long orgId, String name, Long type, Integer year, Boolean active,
			Long clientId, Pageable pageable);

	@Query(value = "SELECT max(db.currentNumber) FROM DocumentBook db WHERE (:typeId is null or db.bookType = :typeId) and (db.clientId = :clientId)")
	Long getMaxCurrentNumberByBookType(Long typeId, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.DocumentBookDto(db.id, db.active, db.name, db.startNumber, db.currentNumber, db.bookType, db.numberOrSign, db.year, db.org.id, db.org.name) FROM DocumentBook db WHERE db.clientId = :clientId ORDER BY db.year DESC")
	Page<DocumentBookDto> findDBByClientId(Long clientId, Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.DocumentBookDto(db.id, db.active, db.name, db.startNumber, db.currentNumber, db.bookType, db.numberOrSign, db.year, db.org.id, db.org.name) "
			+ "FROM DocumentBook db "
			+ "WHERE 1=1 AND (db.orgCreateId = :orgId OR (SELECT count(*)>0 FROM OrgDocBook odb WHERE db.id = odb.bookId AND odb.active = true AND odb.orgId = :orgId) is TRUE) "
			+ "AND db.clientId = :clientId ORDER BY db.year DESC")
	Page<DocumentBookDto> findDBByOrgIdAndClientId(Long orgId, Long clientId, Pageable pageable);

	@Query("SELECT new com.vz.backend.business.domain.DocumentBook(db.id, concat(db.name, ' - ', CAST (db.year AS text)), db.currentNumber, db.bookType, db.numberOrSign, db.year) FROM DocumentBook db WHERE db.bookType = :typeCode AND db.clientId = :clientId")
	List<DocumentBook> findByBookTypeAndClientId(Long typeCode, Long clientId);

	@Transactional
	@Modifying()
	@Query("UPDATE DocumentBook db SET db.active = false WHERE db.active = true AND db.year < :year")
	void lockDocumentBook(int year);

	@Query("SELECT new com.vz.backend.business.domain.DocumentBook(db.id, concat(db.name, ' - ', CAST (db.year AS text)), db.currentNumber, db.bookType, db.numberOrSign, db.year) "
			+ "FROM DocumentBook db INNER JOIN OrgDocBook odb ON db.id = odb.bookId AND odb.active = true "
			+ "WHERE odb.orgId = :orgId AND db.bookType = :typeCode AND db.clientId = :clientId")
	List<DocumentBook> findByBookTypeAndOrgIdAndClientId(Long orgId, Long typeCode, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.DocumentBookDetailDto(db.id, db.active, db.name, db.numberOrSign, db.startNumber, db.currentNumber, db.bookType, db.year, db.org.id, db.org.name) "
			+ "FROM DocumentBook db WHERE db.id = :id AND db.clientId = :clientId")
	DocumentBookDetailDto getDetailById(Long id, Long clientId);

	@Query("SELECT cdb.categoryId FROM CategoryDocBook cdb WHERE cdb.active is true AND cdb.clientId = :clientId AND cdb.bookId = :bookId")
	List<Long> getListSecurityIdsByBookIdAndClientId(Long bookId, Long clientId);
}
