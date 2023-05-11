package com.vz.backend.business.repository;

import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.dto.hstl.ecm.FolderAttachmentDto;
import com.vz.backend.core.config.AttachmentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentOutAttachmentRepository extends IRepository<DocumentOutAttachment> {

	@Modifying
	@Query("DELETE FROM DocumentOutAttachment at WHERE at.docId = :docId")
	void deleteByDocId(@Param("docId") Long docId);

	@Query("SELECT f.name FROM DocumentOutAttachment f WHERE f.docId = :docId")
	List<String> getFileNameByDocId(Long docId);

	List<DocumentOutAttachment> findAllByDocIdIn(List<Long> docIds);

	@Query("SELECT count(*)>0 FROM DocumentOutAttachment d WHERE d.active = true AND d.attachmentType = :type AND d.docId = :docId AND d.clientId = :clientId")
	boolean existAttachmentByTypeAndDocIdAndClientId(AttachmentTypeEnum type, long docId, Long clientId);

	@Query("SELECT count(*)>0 FROM DocumentOutAttachment d WHERE d.active = true AND d.name = :fileName AND d.docId = :docId AND d.clientId = :clientId")
	boolean existAttachmentByFileNameAndDocIdAndClientId(String fileName, long docId, Long clientId);

	Optional<DocumentOutAttachment> findByName(String fileName);

	@Query("SELECT d FROM DocumentOutAttachment d WHERE d.active = true AND d.attachmentType = :type AND d.docId = :docId AND d.clientId = :clientId")
	List<DocumentOutAttachment> findAllByDocIdAndType(AttachmentTypeEnum type, long docId, Long clientId);

	Page<DocumentOutAttachment> findByClientIdAndDocIdAndCreateByAndActive(Long clientId, Long object, Long userId, boolean b, Pageable pageable);

	@Query("SELECT NEW com.vz.backend.business.dto.hstl.ecm.FolderAttachmentDto(a, h.folderId) "
			+ " FROM DocumentOutAttachment a"
			+ " INNER JOIN DocumentOut d ON d.id = a.docId AND d.active=TRUE AND a.active = TRUE AND a.encrypt IS NULL AND a.attachmentType IN (0, 1)"
			+ " INNER JOIN HsFolderDocument h ON h.docId = d.id AND h.type = 'VAN_BAN_DI'"
			+ " WHERE h.folderId IN (:folderIds) AND h.clientId = :clientId ")
	List<FolderAttachmentDto> getByFolderIds(List<Long> folderIds, Long clientId);
}
