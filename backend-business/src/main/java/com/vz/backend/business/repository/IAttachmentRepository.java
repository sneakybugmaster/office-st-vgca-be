package com.vz.backend.business.repository;

import com.vz.backend.business.domain.Attachment;
import com.vz.backend.business.dto.hstl.ecm.FolderAttachmentDto;
import com.vz.backend.core.repository.IRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IAttachmentRepository extends IRepository<Attachment> {
	@Modifying
	@Query("DELETE from Attachment at WHERE at.documentId=:documentId")
	void deleteByDocId(@Param("documentId") Long documentId);

	Attachment findByNameAndClientIdAndActive(String name, Long clientId, boolean active);

	@Query("SELECT at from Attachment at WHERE at.name =:name and at.active = :active and at.clientId =:clientId")
	List<Attachment> findByListNameAndClientIdAndActive(String name, Long clientId, boolean active);

	Attachment findFirstByClientIdAndDocumentIdAndActiveTrue(Long clientId, Long docId);

	List<Attachment> findByDocumentIdAndClientIdAndActiveTrue(Long objId, Long clientId);
	
	@Query("SELECT NEW com.vz.backend.business.dto.hstl.ecm.FolderAttachmentDto(a, h.folderId) "
			+ " FROM Attachment a"
			+ " INNER JOIN Documents d ON d.id = a.documentId AND d.active=TRUE AND a.active = TRUE AND a.encrypt IS NULL"
			+ " INNER JOIN HsFolderDocument h ON h.docId = d.id AND h.type = 'VAN_BAN_DEN'"
			+ " WHERE h.folderId IN (:folderIds) AND h.clientId = :clientId ")
	List<FolderAttachmentDto> getByFolderIds(List<Long> folderIds, Long clientId);

	@Query("SELECT at from Attachment at WHERE at.documentId in (:documentIds)  and at.active is true and at.clientId =:clientId")
	List<Attachment>  findByListDocId(List<Long> documentIds , Long clientId);

	@Query("select a from Attachment a where a.documentId = :docId and a.active = true and a.clientId = :clientId order by a.createDate asc")
	List<Attachment> findByDocIdCreateDateAsc(Long docId, Long clientId);

}
