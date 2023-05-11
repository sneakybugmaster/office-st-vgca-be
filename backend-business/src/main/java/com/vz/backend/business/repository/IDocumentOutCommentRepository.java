package com.vz.backend.business.repository;

import java.util.List;
import java.util.Set;

import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentOutComment;
import com.vz.backend.business.dto.document.DocOutCommentDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentOutCommentRepository extends IRepository<DocumentOutComment> {

	@Query("SELECT new com.vz.backend.business.dto.document.DocOutCommentDto(dc) "
			+ "FROM DocumentOutComment dc WHERE dc.docId = :docId AND dc.clientId = :clientId AND dc.active = :active ORDER BY dc.id DESC")
	List<DocOutCommentDto> findByClientIdAndDocIdAndActive(@Param("clientId") Long clientId,
			@Param("docId") Long docId, @Param("active") Boolean active);
	
	@Query("SELECT new com.vz.backend.business.dto.document.DocOutCommentDto(dc) "
			+ "FROM DocumentOutComment dc WHERE (dc.user.org = :currOrg OR (dc.user.org IN (:subOrgs) AND dc.user.positionModel.isLeadership is true)) "
			+ "AND dc.docId = :docId AND dc.clientId = :clientId AND dc.active = :active ORDER BY dc.id DESC")
	List<DocOutCommentDto> findByCurrOrgAndSubOrgInAndDocIdAndClientIdAndActive(Long currOrg, List<Long> subOrgs, Long clientId, Long docId, Boolean active);

	@Query("SELECT count(*)>0 FROM DocumentOutComment doc INNER JOIN DocumentOutAttachment doa ON doc.active = true AND doa.active = true AND doc.id = doa.cmtId WHERE doc.docId = :docId AND doa.id = :attachId")
	boolean existAttachIdByUserId(Long attachId, Long docId);

	@Query("SELECT doc.docId FROM DocumentOutComment doc WHERE doc.id = :id")
	Long getDocIdById(Long id);

	@Query("SELECT new com.vz.backend.business.dto.document.DocOutCommentDto(dc) "
			+ "FROM DocumentOutComment dc WHERE dc.userId IN (:listUserId) AND dc.docId = :docId AND dc.clientId = :clientId AND dc.active = :active ORDER BY dc.id DESC")
	List<DocOutCommentDto> findByListUserIdAndDocIdAndClientIdAndActive(Set<Long> listUserId, Long clientId, Long docId, boolean active);

	@Query("SELECT count(1) > 0 FROM DocumentOutComment doc where doc.clientId =:clientId and doc.docId =:docId and doc.active =:active")
	boolean existCommentByDocId(Long clientId, Long docId, Boolean active);

	@Query("SELECT doc.docId FROM DocumentOutComment doc " +
			" inner join DocumentOutProcess dop on dop.active =  doc.active and dop.clientId = doc.clientId and dop.docId = doc.docId" +
			" WHERE doc.userId in (:ids) and dop.userId in (:ids) and dop.handleStatus = 'CHO_Y_KIEN' and doc.clientId =:clientId and doc.active =:active ")
	List<Long> getDocIdByUserId(List<Long> ids, Long clientId, Boolean active);

	@Query("SELECT doc.docId FROM DocumentOutComment doc INNER JOIN DocumentOutProcess p ON doc.docId=p.docId " +
			"WHERE doc.userId =:id and doc.clientId =:clientId and doc.active =:active  AND p.handleStatus IN (:status)  ")
	List<Long> getDocIdByUserIdStatus(Long id, Long clientId, Boolean active, DocumentOutHandleStatusEnum[] status);
}
