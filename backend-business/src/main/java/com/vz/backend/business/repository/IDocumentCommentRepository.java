package com.vz.backend.business.repository;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentComment;
import com.vz.backend.business.dto.document.DocInCommentDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentCommentRepository extends IRepository<DocumentComment> {
	@Query("SELECT new com.vz.backend.business.dto.document.DocInCommentDto(dc) "
			+ "FROM DocumentComment dc WHERE dc.clientId = :clientId AND dc.docId in (:docId) AND dc.active = :active ORDER BY dc.createDate DESC")
	List<DocInCommentDto> findByClientIdAndDocIdAndActiveOrderByIdDesc(Long clientId, List<Long> docId, boolean active);

	DocumentComment findByClientIdAndIdAndActive(Long clientId, Long cmtId, boolean active);

	@Query("select new com.vz.backend.business.domain.DocumentComment(d.docId, d.comment) "
			+ " from DocumentComment d where d.docId in (:docId) and d.active = :active "
			+ " and d.clientId =:clientId and (d.docId, d.updateDate) in "
			+ "(select d1.docId, max(d1.updateDate) from DocumentComment d1 "
			+ "where d1.docId in (:docId) and d1.active = :active and d1.clientId =:clientId group by d1.docId)"
			+ " group by d.docId, d.comment")
	List<DocumentComment> findByClientIdAndListDocIdAndActive(Long clientId, List<Long> docId, boolean active);
	
	@Query("SELECT new com.vz.backend.business.dto.document.DocInCommentDto(dc) "
			+ "FROM DocumentComment dc INNER JOIN User u ON u.id = dc.createBy "
			+ "WHERE (u.org = :currOrg OR (u.org IN (:subOrgs) AND u.positionModel.isLeadership is true)) "
			+ "AND dc.clientId = :clientId AND dc.docId = :docId AND dc.active = :active ORDER BY dc.createDate DESC")
	List<DocInCommentDto> findByCurrOrgAndSubOrgInAndDocIdAndClientIdAndActive(Long currOrg, List<Long> subOrgs,
			Long clientId, Long docId, boolean active);
	
	@Query("SELECT new com.vz.backend.business.dto.document.DocInCommentDto(dc) "
			+ "FROM DocumentComment dc "
			+ "WHERE dc.createBy IN (:listUserId) AND dc.docId = :docId AND dc.clientId = :clientId AND dc.active = :active ORDER BY dc.createDate DESC")
	List<DocInCommentDto> findByListUserIdAndDocIdAndClientIdAndActive(Set<Long> listUserId, Long docId, Long clientId, boolean active);

	List<DocumentComment> findByClientIdAndDocIdAndCreateByInAndActiveTrueOrderByIdAsc(Long clientId, Long docId, List<Long> userIds);

	@Query("SELECT dc FROM DocumentComment dc WHERE dc.docId = :docId AND dc.clientId = :clientId AND dc.createDate <= :timestamp AND dc.active IS TRUE ORDER BY dc.createDate DESC")
	List<DocumentComment> findPreviousCommentsByDocIdAndClientId(Long docId, Long clientId, Date timestamp);
}
