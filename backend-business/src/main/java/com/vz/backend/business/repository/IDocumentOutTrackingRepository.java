package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentOutTracking;
import com.vz.backend.core.config.DocumentOutTrackingEnum;

@Repository
public interface IDocumentOutTrackingRepository extends JpaRepository<DocumentOutTracking, Long> {

	@Query("SELECT d FROM DocumentOutTracking d  "
			+ " LEFT JOIN User u ON u.id = d.fromUserId"
			+ " LEFT JOIN User u1 ON u1.id = d.handlerId"
			+ " LEFT JOIN User u2 ON u2.id = d.toUserId"
			+ " WHERE d.docId = :docId" 
			+ " AND ((u IS NULL OR (u IS NOT NULL AND u.active IS TRUE))"
			+ " AND ((u1 IS NULL OR (u1 IS NOT NULL AND u1.active IS TRUE))"
			+ " AND ((u2 IS NULL OR (u2 IS NOT NULL AND u2.active IS TRUE))"
			+ " )))"
			+ " ORDER BY coalesce(d.updateDate, d.createDate) DESC")
	Page<DocumentOutTracking> getListTracking(@Param(value = "docId") Long docId, Pageable pageable);
	
	DocumentOutTracking findFirstByDocIdAndFromUserIdAndActionAndClientIdOrderByIdDesc(Long docId, Long userId,
			DocumentOutTrackingEnum action, Long clientId);
	
	DocumentOutTracking findFirstByDocIdAndToUserIdAndActionAndClientIdOrderByIdDesc(Long docId, Long userId,
			DocumentOutTrackingEnum action, Long clientId);
	
	@Query("SELECT d.fromUserId FROM DocumentOutTracking d WHERE d.docId = :docId AND d.clientId = :clientId AND d.action = 'TRANSFER'")
	List<Long> findListUserTransfer(Long docId, Long clientId);

	List<DocumentOutTracking> findByDocIdAndToUserIdAndClientId(Long docId, Long toUserId, Long clientId);

	@Query("SELECT t.fromUserId FROM DocumentOutTracking t WHERE t.fromUserId != null AND t.docId = :docId AND t.toUserId = :toUserId AND t.clientId = :clientId")
	List<Long> findFromUserByDocIdAndToUserIdAndClientId(Long docId, Long toUserId, Long clientId);

	@Query("SELECT t.toUserId FROM DocumentOutTracking t WHERE t.toUserId != null AND t.fromUserId IN (:fromUser) AND t.docId = :docId AND t.clientId = :clientId")
	List<Long> findToUserByFromUserInAndDocIdAndClientId(List<Long> fromUser, Long docId, Long clientId);
	
	@Query("SELECT t FROM DocumentOutTracking t" + " WHERE t.clientId = :clientId "
			+ " AND t.docId = :docId ORDER BY COALESCE(t.updateDate, t.createDate) DESC")
	List<DocumentOutTracking> getListTracking(Long docId, Long clientId);

	DocumentOutTracking findFirstByDocIdAndHandlerIdAndActionAndClientIdOrderByIdDesc(Long docId, Long handlerId,
			DocumentOutTrackingEnum incoming, Long clientId);

	List<DocumentOutTracking> findByDocIdAndFromUserIdAndClientIdAndAndAction(Long docId, Long userId,
			Long clientId, DocumentOutTrackingEnum action);
}
