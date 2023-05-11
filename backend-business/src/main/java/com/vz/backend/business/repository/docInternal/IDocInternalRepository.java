package com.vz.backend.business.repository.docInternal;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.config.DocInternalHandleEnum;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.config.DocInternalApproveStatusEnum;
import com.vz.backend.business.config.DocInternalApproveTypeEnum;
import com.vz.backend.business.domain.documentInternal.DocumentInternal;
import com.vz.backend.business.dto.document.DocInternalForListDto;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocInternalRepository extends IRepository<DocumentInternal> {

	@Query("UPDATE DocumentInternal d SET d.active = :active WHERE d.id = :id AND d.createBy = :userId")
	void setActiveByIdAndCreateBy(boolean active, Long id, Long userId);

	@Query("SELECT d FROM DocumentInternal d WHERE d.id = :id AND d.active = true AND d.clientId = :clientId")
	DocumentInternal getDetailById(Long id, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.document.DocInternalForListDto(d1.id, d1.numberOrSign, d1.preview, d1.status, d1.createBy, d1.createUser.fullName, d1.createDate, d1.signDate, d1.docDate, d1.read) "
			+ "FROM DocumentInternal d1 WHERE d1.id IN (SELECT d.id "
			+ "FROM DocumentInternal d LEFT JOIN DocInternalApprove a ON a.docId = d.id AND a.active is true "
			+ "INNER JOIN User u ON u.id = :userId "
			+ "LEFT JOIN DocInternalReceiver dr ON dr.userId = u.id AND dr.active is true "
			+ "WHERE d.status IN (:dStatus) AND d.active is true AND d.clientId = :clientId "
			+ "AND ( :checkInternal IS NULL or (CONCAT(:checkInternal, '') = '1' and d.id = dr.docId) or (CONCAT(:checkInternal, '') = '2' and (a.type <> 'RECEIVER' OR d.status = 'NB_HOAN_THANH'))  ) "
			+ "AND (d.createBy = :createBy OR (a.type <> :signer AND a.handleStatus IN (:pStatus) AND (a.userId = :userId OR (a.orgId = u.org AND u.positionModel.isLeadership is true and u.lead is true ))) "

			+ "OR (a.type = :signer AND a.handleStatus IN (:pStatus) AND a.userId = :userId AND d.status IN (:ldStatus))) "
			+ "AND (:numberOrSign is null OR lower(d.numberOrSign) like %:numberOrSign%) "
			+ "AND (:preview is null OR lower(d.preview) like %:preview%) "
			+ "AND (:personEnter is null OR lower(d.createUser.fullName) like %:personEnter%) "
			+ "AND (coalesce(:createDate, null) is null OR d.createDate = :createDate) "
			+ "AND (coalesce(:createFrom, null) is null OR d.createDate >= :createFrom) "
			+ "AND (coalesce(:createTo, null) is null OR d.createDate <= :createTo) "
			+ "AND (coalesce(:approveDate, null) is null OR d.approveDate = :approveDate) "
			+ "AND (coalesce(:approveFrom, null) is null OR d.approveDate >= :approveFrom) "
			+ "AND (coalesce(:approveTo, null) is null OR d.approveDate <= :approveTo))")
	Page<DocInternalForListDto> getListDocInternal(Long userId, Long createBy,
												   List<DocInternalApproveStatusEnum> pStatus, List<DocumentStatusEnum> dStatus, String numberOrSign,
												   String preview, String personEnter, Date createDate, Date createFrom, Date createTo, Date approveDate,
												   Date approveFrom, Date approveTo, Long clientId, List<DocumentStatusEnum> ldStatus,
												   DocInternalApproveTypeEnum signer, Long checkInternal,  Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.document.DocInternalForListDto(d1.id, d1.numberOrSign, d1.preview, d1.status, d1.createBy, d1.createUser.fullName, d1.createDate, d1.signDate, d1.docDate, d1.read) "
			+ "FROM DocumentInternal d1 WHERE d1.id IN (SELECT d.id "
			+ "FROM DocumentInternal d LEFT JOIN DocInternalApprove a ON a.docId = d.id AND a.active is true "
			+ "INNER JOIN User u ON u.id = :userId "
			+ "LEFT JOIN ObjectTag ot ON ot.objId = d.id AND ot.active = TRUE AND ot.type = 'VAN_BAN_NOI_BO' "
			+ "LEFT JOIN Tag t ON t.id = ot.tagId AND t.active = TRUE AND t.createBy = :userId "
			+ "WHERE (:status is null OR d.status = :status) AND d.active is true AND d.clientId = :clientId "
			+ "AND (d.createBy = :createBy OR (a.type <> :signer AND (a.userId = :userId OR (a.orgId = u.org AND u.lead is true))) "
			+ "OR (a.type = :signer AND a.userId = :userId AND d.status IN (:ldStatus))) "
			+ "AND (:numberOrSign is null OR lower(d.numberOrSign) like %:numberOrSign% OR lower(t.name) like %:numberOrSign%) "
			+ "AND (:preview is null OR lower(d.preview) like %:preview% OR lower(t.name) like %:preview%) "
			+ "AND (:personEnter is null OR lower(d.createUser.fullName) like %:personEnter%) "
			+ "AND (coalesce(:createDate, null) is null OR d.createDate = :createDate) "
			+ "AND (coalesce(:createFrom, null) is null OR d.createDate > :createFrom) "
			+ "AND (coalesce(:createTo, null) is null OR d.createDate < :createTo) "
			+ "AND (coalesce(:approveDate, null) is null OR d.approveDate = :approveDate) "
			+ "AND (coalesce(:approveFrom, null) is null OR d.approveDate > :approveFrom) "
			+ "AND (coalesce(:approveTo, null) is null OR d.approveDate < :approveTo))")
	Page<DocInternalForListDto> getAllDocInternal(Long userId, Long createBy, String numberOrSign,
			DocumentStatusEnum status, String preview, String personEnter, Date createDate, Date createFrom,
			Date createTo, Date approveDate, Date approveFrom, Date approveTo, Long clientId,
			List<DocumentStatusEnum> ldStatus, DocInternalApproveTypeEnum signer, Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.document.DocInternalForListDto(d1.id, d1.numberOrSign, d1.preview, d1.status, d1.createBy, d1.createUser.fullName, d1.createDate, d1.signDate, d1.docDate, d1.read) "
			+ "FROM DocumentInternal d1 WHERE d1.id IN (SELECT d.id "
			+ "FROM DocumentInternal d LEFT JOIN DocInternalApprove a ON a.docId = d.id AND a.active is true "
			+ "INNER JOIN User u ON u.id = :userId "
			+ "LEFT JOIN ObjectTag ot ON ot.objId = d.id AND ot.active = TRUE AND ot.type = 'VAN_BAN_NOI_BO' "
			+ "LEFT JOIN Tag t ON t.id = ot.tagId AND t.active = TRUE AND t.createBy = :userId "
			+ "WHERE (:status is null OR d.status = :status) AND d.active is true AND d.clientId = :clientId "
			+ "AND (d.createBy = :createBy OR (a.type <> :signer AND (a.userId = :userId OR (a.orgId = u.org AND u.position = :managerId))) "
			+ "OR (a.type = :signer AND a.userId = :userId AND d.status IN (:ldStatus))) "
			+ "AND (:numberOrSign is null OR lower(d.numberOrSign) like %:numberOrSign% OR lower(t.name) like %:numberOrSign%) "
			+ "AND (:preview is null OR lower(d.preview) like %:preview% OR lower(t.name) like %:preview%) "
			+ "AND (:personEnter is null OR lower(d.createUser.fullName) like %:personEnter%) "
			+ "AND (coalesce(:createDate, null) is null OR d.createDate = :createDate) "
			+ "AND (coalesce(:createFrom, null) is null OR d.createDate > :createFrom) "
			+ "AND (coalesce(:createTo, null) is null OR d.createDate < :createTo) "
			+ "AND (coalesce(:approveDate, null) is null OR d.approveDate = :approveDate) "
			+ "AND (coalesce(:approveFrom, null) is null OR d.approveDate > :approveFrom) "
			+ "AND (coalesce(:approveTo, null) is null OR d.approveDate < :approveTo))")
	Page<DocInternalForListDto> getAllDocInternalWithTab7(Long managerId, Long userId, Long createBy, String numberOrSign,
			DocumentStatusEnum status, String preview, String personEnter, Date createDate, Date createFrom,
			Date createTo, Date approveDate, Date approveFrom, Date approveTo, Long clientId,
			List<DocumentStatusEnum> ldStatus, DocInternalApproveTypeEnum signer, Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.document.DocInternalForListDto(d1.id, d1.numberOrSign, d1.preview, d1.status, d1.createBy, d1.createUser.fullName, d1.createDate, d1.signDate, d1.docDate, d1.read) " +
			"FROM DocumentInternal d1 inner join DocInternalReceiver a on a.docId=d1.id " +
			"inner join User u on a.userId=u.id where " +
			"d1.active = true  and a.active=true and u.active=true " +
			"and d1.clientId=:clientId and u.clientId=:clientId and a.clientId=:clientId " +
			"and u.id=:userId and d1.status in (:ldStatus)")
	Page<DocInternalForListDto> getAllDocComplete(Long userId, Long clientId,
												  List<DocumentStatusEnum> ldStatus, Pageable pageable);

	@Query("SELECT count(*) > 0 FROM DocumentInternal d INNER JOIN User u ON u.id = :userId "
			+ "LEFT JOIN DocInternalApprove a ON a.docId = d.id AND a.active is true "
			+ "WHERE d.id = :docId AND (d.createBy = :userId OR a.userId = :userId OR (a.orgId = u.org AND u.positionModel.isLeadership is true))")
	boolean checkPermission(Long docId, Long userId);

	@Query("SELECT count(DISTINCT d.id) "
			+ "FROM DocumentInternal d LEFT JOIN DocInternalApprove a ON a.docId = d.id AND a.active is true "
			+ "INNER JOIN User u ON u.id = :userId "
			+ "WHERE d.status IN (:dStatus) AND d.active is true AND d.clientId = :clientId "
			+ "AND (d.createBy = :createBy OR (a.type <> :signer AND a.handleStatus IN (:pStatus) AND (a.userId = :userId OR (a.orgId = u.org AND u.positionModel.isLeadership is true))) "
			+ "OR (a.type = :signer AND a.handleStatus IN (:pStatus) AND a.userId = :userId AND d.status IN (:ldStatus))) ")
	Long report(Long userId, Long createBy, List<DocInternalApproveStatusEnum> pStatus,
			List<DocumentStatusEnum> dStatus, Long clientId, List<DocumentStatusEnum> ldStatus,
			DocInternalApproveTypeEnum signer);

	@Query("SELECT count(DISTINCT d.id) "
			+ "FROM DocumentInternal d LEFT JOIN DocInternalApprove a ON a.docId = d.id AND a.active is true "
			+ "INNER JOIN User u ON u.id = :userId "
			+ "INNER JOIN DocInternalReceiver dr ON dr.userId = u.id AND d.id = dr.docId AND dr.active is true "
			+ "WHERE d.status IN (:dStatus) AND d.active is true AND d.clientId = :clientId "
			+ "AND (d.createBy = :createBy OR (a.type <> :signer AND a.handleStatus IN (:pStatus) AND (a.userId = :userId OR (a.orgId = u.org AND u.positionModel.isLeadership is true and u.lead is true ))) "

			+ "OR (a.type = :signer AND a.handleStatus IN (:pStatus) AND a.userId = :userId AND d.status IN (:ldStatus))) ")
	Long reportVBXL(Long userId, Long createBy,
													List<DocInternalApproveStatusEnum> pStatus, List<DocumentStatusEnum> dStatus, Long clientId, List<DocumentStatusEnum> ldStatus,
													DocInternalApproveTypeEnum signer);

	@Query("SELECT count(*) FROM DocumentInternal d WHERE d.orgCreateId = :orgId AND d.clientId = :clientId")
	Long countTotalDocByOrgIdAndClientId(Long orgId, Long clientId);

	@Query("SELECT count(*) > 0 FROM DocumentInternal d WHERE d.numberOrSign = :numberOrSign AND d.clientId = :clientId AND d.active = TRUE and d.orgCreateId = :orgId")
	boolean findByNumberOrSign(Long clientId, String numberOrSign, Long orgId);

	@Query("SELECT COUNT(*) > 0 FROM DocInternalReceiver d WHERE  d.docId =:docId AND d.userId =:userId AND d.clientId =:clientId and d.handleStatus = 'EXECUTE' AND d.type ='USER'")
	boolean findByExecuteDocinternal(Long clientId, Long userId, Long docId);

}