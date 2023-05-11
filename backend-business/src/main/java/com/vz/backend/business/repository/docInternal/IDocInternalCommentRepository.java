package com.vz.backend.business.repository.docInternal;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.documentInternal.DocInternalComment;
import com.vz.backend.business.dto.document.CommentDto;
import com.vz.backend.business.dto.document.DocInCommentDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocInternalCommentRepository extends IRepository<DocInternalComment> {

	@Query("SELECT new com.vz.backend.business.dto.document.DocInCommentDto(dc.id, dc.approveId, dc.createUser.fullName, dc.createUser.positionModel.name, dc.comment, dc.createDate, dc.handleStatus) "
			+ "FROM DocInternalComment dc WHERE dc.approveId IN (:listApproveId) AND dc.active is true AND dc.handleStatus = 'DA_DUYET'")
	List<DocInCommentDto> getListCommentByApproveId(List<Long> listApproveId);

	@Query("SELECT new com.vz.backend.business.dto.document.CommentDto(dc.id, dc.approveId, dc.createUser.fullName, dc.createUser.positionModel.name, dc.comment, dc.createDate, dc.handleStatus) "
			+ "FROM DocInternalComment dc JOIN DocInternalApprove da ON dc.approveId = da.id JOIN DocumentInternal d ON da.docId = d.id "
			+ "WHERE d.id = :docInternalId AND dc.clientId = :clientId AND dc.active is true AND dc.handleStatus = 'BINH_LUAN' ORDER BY dc.createDate DESC")
	List<CommentDto> findAllDocInternalComments(Long clientId, Long docInternalId);

	@Query("SELECT new com.vz.backend.business.dto.document.CommentDto(dc.id, dc.approveId, dc.createUser.fullName, dc.createUser.positionModel.name, dc.comment, dc.createDate, dc.handleStatus) "
			+ "FROM DocInternalComment dc JOIN DocInternalApprove da ON dc.approveId = da.id JOIN DocumentInternal d ON da.docId = d.id "
			+ "WHERE d.id = :docInternalId AND dc.clientId = :clientId AND dc.active is true AND dc.createBy = :currentUserId ORDER BY dc.createDate DESC")
	List<CommentDto> findDocCommentByUser(Long clientId, Long currentUserId, Long docInternalId);

}
