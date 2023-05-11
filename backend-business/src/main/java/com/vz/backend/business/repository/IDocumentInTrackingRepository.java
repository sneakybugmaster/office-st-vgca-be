package com.vz.backend.business.repository;

import java.util.List;

import com.vz.backend.core.config.DocumentInTrackingEnum;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentInTracking;
import com.vz.backend.business.dto.FollowDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentInTrackingRepository extends IRepository<DocumentInTracking> {

	@Query("select new com.vz.backend.business.dto.FollowDto(t.userId, t.orgName, t.user.fullName, t.user.positionModel.name, t.action, t.docType, t.createDate) "
			+ " from DocumentInTracking t" + " where t.clientId = :clientId " + " and t.active = true "
			+ " and t.docId = :docId")
	Page<FollowDto> findByIdAndClientId(Long clientId, Long docId, Pageable page);
	
	@Query("SELECT NEW com.vz.backend.business.dto.FollowDto(t.userId, t.orgName, t.user.fullName, t.user.positionModel.name, t.action, t.docType, t.createDate) "
			+ " FROM DocumentInTracking t" + " WHERE t.clientId = :clientId " + " AND t.active = true "
			+ " AND t.docId = :docId ORDER BY COALESCE(t.updateDate, t.createDate) DESC")
	List<FollowDto> findByIdAndClientId(Long clientId, Long docId);

	@Query("SELECT t.userId "
			+ " FROM DocumentInTracking t WHERE t.clientId = :clientId AND t.active = true and t.action in (:actions)"
			+ " AND t.docId = :docId group by t.userId")
	List<Long> getUserIdByDocIdAndActions(Long clientId, Long docId, List<DocumentInTrackingEnum> actions);
}
