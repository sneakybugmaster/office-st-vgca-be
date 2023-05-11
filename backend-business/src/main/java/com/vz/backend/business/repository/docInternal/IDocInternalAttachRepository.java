package com.vz.backend.business.repository.docInternal;

import java.util.List;

import javax.transaction.Transactional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.documentInternal.DocInternalAttach;
import com.vz.backend.business.dto.document.DocInternalAttachDto;
import com.vz.backend.core.config.AttachmentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocInternalAttachRepository extends IRepository<DocInternalAttach> {

	@Query("SELECT new com.vz.backend.business.dto.document.DocInternalAttachDto(d.id, d.name, d.type, d.size, d.attachType, d.encrypt) "
			+ "FROM DocInternalAttach d WHERE d.docId = :docId AND d.active is true AND d.clientId = :clientId")
	List<DocInternalAttachDto> getListAttachByDocIdAndClientId(Long docId, Long clientId);

	@Transactional
	@Modifying
	@Query("UPDATE DocInternalAttach d SET d.active = false WHERE d.id IN (:deleteIds) AND d.clientId = :clientId")
	void deactiveByIdInAndClientId(List<Long> deleteIds, Long clientId);

	Page<DocInternalAttach> findByClientIdAndDocIdAndCreateByAndAttachTypeAndActive(Long clientId, Long object,
			Long userId, AttachmentTypeEnum draft, boolean b, Pageable pageable);

	List<DocInternalAttach> findByClientIdAndDocInternalCommentIdAndAttachTypeAndActiveTrue(Long clientId, Long id,
			AttachmentTypeEnum binhLuan);

	DocInternalAttach findFirstByClientIdAndNameAndActiveTrue(String fileName, Long clientId);
}
