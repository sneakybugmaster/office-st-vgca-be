package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.ObjectTag;
import com.vz.backend.business.dto.ObjectTagDto;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IObjectTagRepository extends IRepository<ObjectTag> {

	ObjectTag findByClientIdAndTagIdAndObjIdAndType(Long clientId, Long tagId, Long docId, DocumentTypeEnum type);

	List<ObjectTag> findByClientIdAndTagIdAndActiveTrue(Long clientId, Long tagId);
	
	@Query("SELECT new com.vz.backend.business.dto.ObjectTagDto(ot.id, ot.objId, ot.tagId, ot.type, ot.tag.name, ot.previewName, ot.issueDate) FROM ObjectTag ot " +
			" WHERE ot.clientId = :clientId AND ot.tagId = :tagId AND ot.active = 'TRUE' " +
			" AND (:name is null or lower(ot.previewName) like %:name% ) "
			+ "ORDER BY (CASE WHEN ot.issueDate IS NULL THEN 1 ELSE 2 END) DESC")
	Page<ObjectTagDto> findByClientIdAndTagIdAndActiveTrue(Long clientId, Long tagId, String name, Pageable pageable);

	ObjectTag findByClientIdAndObjIdAndTagIdAndTypeAndActiveTrue(Long clientId, Long objId, Long tagId,
			DocumentTypeEnum type);

	List<ObjectTag> findByClientIdAndObjIdAndTypeAndActiveTrue(Long clientId, Long objId, DocumentTypeEnum type);

}
