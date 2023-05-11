package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.TemplateDocument;
import com.vz.backend.core.config.CalendarStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ITemplateDocumentRepository extends IRepository<TemplateDocument> {
	
	List<TemplateDocument> findByClientIdAndStatusAndActiveTrue(Long clientId, CalendarStatusEnum approve);

	@Query("SELECT t FROM TemplateDocument t WHERE t.clientId=:clientId "
			+ " AND (:status IS NULL OR t.status =:status) "
			+ " AND t.active=TRUE AND (:type IS NULL OR t.docType =:type) "
			+ " AND (:name IS NULL OR lower(t.name) LIKE %:name%) OR LOWER(t.tName) LIKE %:name%")
	Page<TemplateDocument> all(CalendarStatusEnum status, DocumentTypeEnum type, String name, Long clientId,
			Pageable page);
}
