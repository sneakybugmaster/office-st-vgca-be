package com.vz.backend.business.repository.docInternal;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.DocInternalTrackingEnum;
import com.vz.backend.business.domain.documentInternal.DocInternalTracking;
import com.vz.backend.business.dto.document.ApproverDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocInternalTrackingRepository extends IRepository<DocInternalTracking> {
	@Query("SELECT new com.vz.backend.business.dto.document.ApproverDto(dt.id, u.id, u.fullName, c.name, dc.comment, dc.createDate) "
			+ "FROM DocInternalTracking dt LEFT JOIN User u ON u.id = dt.fromUserId LEFT JOIN Category c ON c.id = u.position "
			+ "LEFT JOIN DocInternalComment dc ON dc.id = dt.commentId AND dc.active is true "
			+ "WHERE dt.docId = :docId AND dt.action = :action AND dt.active is true AND dt.clientId = :clientId")
	List<ApproverDto> findByDocIdAndAction(Long docId, DocInternalTrackingEnum action, Long clientId);

}
