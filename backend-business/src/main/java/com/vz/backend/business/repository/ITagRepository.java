package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.Tag;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ITagRepository extends IRepository<Tag> {

	Tag findByClientIdAndCreateByAndNameAndActiveTrue(Long clientId, Long userId, String name);

	List<Tag> findByClientIdAndCreateByAndActiveTrue(Long clientId, Long userId);
	
	Page<Tag> findByClientIdAndCreateByAndActiveTrue(Long clientId, Long userId, Pageable pageable);

	@Query("SELECT t FROM Tag t WHERE t.clientId = :clientId AND t.createBy = :userId AND t.active = 'TRUE' AND (:text IS NULL OR LOWER(t.name) LIKE %:text%) ORDER BY t.name ASC, t.createDate DESC")
	Page<Tag> searchTagByName(Long clientId, Long userId, String text, Pageable pageable);
	
	@Query("SELECT t FROM Tag t WHERE t.clientId = :clientId AND t.createBy = :userId AND t.active = 'TRUE' AND (:text IS NULL OR LOWER(t.name) LIKE %:text%) ORDER BY t.name ASC, t.createDate DESC")
	List<Tag> searchTagByName(Long clientId, Long userId, String text);


	Page<Tag> findByClientIdAndCreateByAndActiveTrueOrderByNameAscCreateDateDesc(Long clientId, Long userId,
			Pageable pageable);

	List<Tag> findByClientIdAndCreateByAndActiveTrueOrderByNameAscCreateDateDesc(Long clientId, Long userId);

	@Query("SELECT t FROM Tag t JOIN ObjectTag o ON t.id = o.tagId WHERE t.clientId = :clientId AND t.active = 'TRUE' AND o.clientId = :clientId AND o.objId = :objId AND o.type = :type AND o.active = 'TRUE' AND t.createBy = :userId ORDER BY t.name ASC, t.createDate DESC")
	List<Tag> findByClientIdAndObjIdAndTypeAndActiveTrue(Long clientId, Long objId, DocumentTypeEnum type, Long userId);
}
