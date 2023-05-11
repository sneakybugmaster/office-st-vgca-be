package com.vz.backend.business.repository.docInternal;

import java.util.List;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.DocInternalApproveStatusEnum;
import com.vz.backend.business.config.DocInternalApproveTypeEnum;
import com.vz.backend.business.domain.documentInternal.DocInternalApprove;
import com.vz.backend.business.dto.document.ApproverDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocInternalApproveRepository extends IRepository<DocInternalApprove> {

	List<DocInternalApprove> findByDocIdAndClientId(Long docId, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.document.ApproverDto(d.id, d.type, d.handleStatus, u.id, u.fullName, c.name, o.id, o.name, d.lastComment) "
			+ "FROM DocInternalApprove d LEFT JOIN User u ON u.id = d.userId LEFT JOIN Category c ON c.id = u.position "
			+ "LEFT JOIN Organization o ON o.id = d.orgId "
			+ "WHERE d.docId = :docId AND d.active is true AND d.clientId = :clientId AND d.type != 'CREATOR' AND d.type != 'RECEIVER'")
	List<ApproverDto> getListApproverByDocIdAndClientId(Long docId, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.document.ApproverDto(d.id, d.type, d.handleStatus, u.id, u.fullName, c.name, o.id, o.name, d.lastComment) " +
			"FROM DocInternalApprove d LEFT JOIN User u ON u.id = d.userId " +
			"LEFT JOIN Category c ON c.id = u.position " +
			"LEFT JOIN Organization o ON o.id = d.orgId " +
			"WHERE d.docId = :docId AND d.type = :type AND d.handleStatus = :handleStatus " +
			"AND d.active = :active AND d.clientId = :clientId")
	List<ApproverDto> getUserIdByDocIdAndTypeAndStatusAndActiveAndClientId(Long docId, DocInternalApproveTypeEnum type,
																			DocInternalApproveStatusEnum handleStatus, boolean active, Long clientId);

	@Query("SELECT d FROM DocInternalApprove d WHERE d.docId = :docId AND (d.userId = :userId OR (:orgId is not null AND d.orgId = :orgId)) AND d.handleStatus IN (:handleStatus) AND d.clientId = :clientId AND d.active is true")
	List<DocInternalApprove> findByDocIdAndUserIdOrOrgIdAndHandleStatusInAndClientId(Long docId, Long userId,
			Long orgId, List<DocInternalApproveStatusEnum> handleStatus, Long clientId);

	@Query("SELECT d FROM DocInternalApprove d WHERE d.docId = :docId AND d.handleStatus IN (:handleStatus) AND d.clientId = :clientId AND d.active is true")
	List<DocInternalApprove> findByDocIdAndHandleStatusInAndClientId(Long docId,
			List<DocInternalApproveStatusEnum> handleStatus, Long clientId);

	@Query("SELECT d FROM DocInternalApprove d WHERE d.docId = :docId AND d.userId = :userId AND d.handleStatus = :handleStatus AND d.type = :type AND d.clientId = :clientId AND d.active is true")
	DocInternalApprove findByDocIdAndUserIdAndHandleStatusAndTypeAndClientId(Long docId, Long userId,
			DocInternalApproveStatusEnum handleStatus, DocInternalApproveTypeEnum type, Long clientId);

	@Transactional
	@Modifying
	@Query("UPDATE DocInternalApprove d SET d.handleStatus = :status WHERE d.docId = :docId AND d.clientId = :clientId")
	void updateStatusByDocIdAndClientId(DocInternalApproveStatusEnum status, Long docId, Long clientId);

	@Query("SELECT d.userId FROM DocInternalApprove d WHERE d.docId = :docId AND d.type = :type AND d.handleStatus = :handleStatus AND d.active = :active AND d.clientId = :clientId")
	Long findUserIdByDocIdAndTypeAndStatusAndActiveAndClientId(Long docId, DocInternalApproveTypeEnum type,
			DocInternalApproveStatusEnum handleStatus, boolean active, Long clientId);

	List<DocInternalApprove> findByDocIdAndClientIdAndActiveTrue(Long docId, Long clientId);

	@Query("SELECT da FROM DocInternalApprove da WHERE da.clientId = :clientId AND da.docId = :docId AND ((da.userId = :userId AND da.orgId IS NULL) OR (da.orgId = :orgId AND da.userId IS NULL))")
	List<DocInternalApprove> findByClientIdAndUserIdOrOrgIdAndDocId(Long clientId, Long userId, Long orgId, Long docId);

	@Query("SELECT da.userId FROM DocInternalApprove da JOIN DocumentInternal d ON da.docId = d.id WHERE da.clientId = :clientId AND da.docId = :id AND da.type = 'CREATOR'")
	Long findCreatorByDocId(Long clientId, Long id);

	@Query("SELECT d FROM DocInternalApprove d WHERE d.docId = :docId AND d.userId = :userId AND  d.type = :type AND d.clientId = :clientId AND d.active is true")
	DocInternalApprove findByDocIdAndUserIdAndTypeAndClientId(Long docId, Long userId, DocInternalApproveTypeEnum type, Long clientId);

	@Query("SELECT d FROM DocInternalApprove d WHERE d.docId = :docId AND d.userId = :userId AND d.type in ('RECEIVER', 'CREATOR') AND d.clientId = :clientId AND d.active is true")
	DocInternalApprove findByDocIdAndUserIdAndTypeReceiverAndClientId(Long docId, Long userId, Long clientId);
}