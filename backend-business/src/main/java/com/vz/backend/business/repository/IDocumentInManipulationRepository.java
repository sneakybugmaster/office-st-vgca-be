package com.vz.backend.business.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentInManipulation;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.dto.LabelValueDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentInManipulationRepository extends IRepository<DocumentInManipulation> {
	List<DocumentInManipulation> findByDocIdAndToUserAndHandleStatusAndClientIdAndActive(Long docId, Long userId, DocumentInHandleStatusEnum handleStatus, Long clientId, boolean active);
	
	@Query("SELECT m.toUser FROM DocumentInManipulation m "
			+ "WHERE m.frUser = :frUser AND m.toUser IN (:toUsers) "
			+ "AND m.clientId = :clientId AND m.active = :active AND m.docId = :docId "
			+ "AND m.handleStatus = :handleStatus")
	List<Long> findByDocIdAndFrUserAndToUsers(Long docId, Long frUser, List<Long> toUsers, DocumentInHandleStatusEnum handleStatus, Long clientId, boolean active);

	@Query("SELECT m FROM DocumentInManipulation m "
			+ "WHERE (:toUser IS NULL OR m.toUser = :toUser) "
			+ "AND m.clientId = :clientId "
			+ "AND m.active = :active "
			+ "AND (:frUser IS NULL OR m.frUser = :frUser) "
			+ "AND m.doc.status IN ('DOING', 'RETURN_DOC', 'DONE') "
			+ "AND (:text IS NULL OR LOWER(m.doc.numberOrSign) LIKE %:text% OR LOWER(m.doc.preview) LIKE %:text%) "
			+ "AND (m.docId, m.frUser, m.toUser) IN ("
			+ "		SELECT m1.docId, m1.frUser, m1.toUser FROM DocumentInManipulation m1 "
			+ "		WHERE (:toUser IS NULL OR m1.toUser = :toUser) AND (:frUser IS NULL OR m1.frUser = :frUser) "
			+ "		AND m1.clientId = :clientId AND m1.active = :active "
			+ "		GROUP BY m1.docId, m1.frUser, m1.toUser) ")
	Page<DocumentInManipulation> findByUserId(String text, Long toUser, Long frUser, Long clientId, boolean active, Pageable pageable);

	@Query("SELECT count(1)>0 FROM DocumentInManipulation m WHERE m.toUser = :toUser AND m.handleStatus ='CHO_CHO_Y_KIEN' AND m.active = TRUE AND m.clientId = :clientId AND m.docId=:docId")
	boolean hasAskToUser(Long docId, Long toUser, Long clientId);
	
	@Query("SELECT NEW com.vz.backend.core.dto.LabelValueDto(COUNT(1), m.docId) FROM DocumentInManipulation m WHERE m.toUser = :toUser AND m.handleStatus ='CHO_CHO_Y_KIEN' AND m.active = TRUE AND m.clientId = :clientId AND m.docId IN (:docIds) GROUP BY  m.docId")
	List<LabelValueDto<Long>> hasAskToUser(List<Long> docIds, Long toUser, Long clientId);

	@Query("SELECT count(1)>0 FROM DocumentInManipulation m WHERE (m.toUser = :userId OR m.frUser = :userId) AND m.active = TRUE AND m.clientId = :clientId AND m.docId=:docId")
	boolean hasRelatedIdea(Long docId, Long userId, Long clientId);

	@Query("SELECT count(*) > 0 FROM DocumentInManipulation m WHERE (m.toUser = :userId OR m.frUser =:userId) AND m.active = TRUE AND m.clientId = :clientId AND m.docId =:docId")
	boolean getHandleStatusByDocId(Long userId, Long docId, Long clientId);

	@Query("SELECT m.frUser FROM DocumentInManipulation m WHERE m.toUser = :userId AND m.docId = :docId AND m.clientId = :clientId AND m.active = :active")
	List<Long> findFrUserByToUserAndDocIdAndClientIdAndActive(Long userId, Long docId, Long clientId, boolean active);

	@Query("SELECT m.toUser FROM DocumentInManipulation m WHERE m.frUser = :userId AND m.docId = :docId AND m.clientId = :clientId AND m.active = :active")
	List<Long> findToUserByFrUserAndDocIdAndClientIdAndActive(Long userId, Long docId, Long clientId, boolean active);

	@Query("SELECT COUNT(1) FROM DocumentInManipulation m "
			+ " WHERE (:userId IS NULL OR m.toUser = :userId) "
			+ " AND m.clientId = :clientId "
			+ " AND m.active = TRUE "
			+ " AND m.doc.status IN ('DOING', 'RETURN_DOC', 'DONE') "
			+ " AND (COALESCE(:startDate, NULL) IS NULL OR COALESCE(:endDate, NULL) IS NULL OR COALESCE(m.updateDate, m.createDate) IS NULL OR m.updateDate BETWEEN :startDate AND :endDate)"
			+ " AND (m.docId, m.frUser, m.toUser) IN ("
			+ "		SELECT m1.docId, m1.frUser, m1.toUser FROM DocumentInManipulation m1 "
			+ "		WHERE (:userId IS NULL OR m1.toUser = :userId)"
			+ "		AND m1.clientId = :clientId AND m1.active = TRUE "
			+ "		GROUP BY m1.docId, m1.frUser, m1.toUser) ")
	long waitComment(Long userId, Date startDate, Date endDate, Long clientId);

	@Query("SELECT m.toUser FROM DocumentInManipulation m WHERE m.clientId = :clientId AND m.active = TRUE AND m.docId IN (:docIds) ")
	List<Long> findRelatedByDocId(List<Long> docIds, Long clientId);
}
