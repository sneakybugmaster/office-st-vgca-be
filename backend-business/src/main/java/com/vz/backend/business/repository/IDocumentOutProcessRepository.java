package com.vz.backend.business.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.vz.backend.business.domain.DocumentOutProcess;
import com.vz.backend.business.dto.DocumentOutProcessDto;
import com.vz.backend.business.dto.DocumentProcessDto;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.business.dto.fullreport.SimpleProcessOut;
import com.vz.backend.business.dto.kpi.KPIDataDto;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentOutProcessRepository extends IRepository<DocumentOutProcess> {

	@Query("Select d from DocumentOutProcess d where (:active is null OR d.active = :active) AND d.docId = :docId AND d.userId = :userId AND (:clientId is null or d.clientId = :clientId) ORDER BY d.id DESC")
	List<DocumentOutProcess> findByDocIdAndUserIdAndClientId(Boolean active, Long docId,
			Long userId, Long clientId);

	DocumentOutProcess findFirstByActiveAndDocIdAndUserIdAndClientIdOrderByIdDesc(Boolean active, Long docId,
			Long userId, Long clientId);

	@Query("Select d from DocumentOutProcess d where (:active is null OR d.active = :active) AND d.docId = :docId AND d.userId = :userId AND d.handleStatus = :handleStatus ORDER BY d.id DESC")
	List<DocumentOutProcess> getByDocIdAndUserIdAndHandleStatus(Boolean active, Long docId, Long userId,
			DocumentOutHandleStatusEnum handleStatus);

	DocumentOutProcess findFirstByActiveAndDocIdAndUserIdAndHandleStatusOrderByIdDesc(Boolean active, Long docId,
			Long userId, DocumentOutHandleStatusEnum handleStatus);

	DocumentOutProcess findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(Boolean active, Long docId,
			Long userId, DocumentOutHandleStatusEnum[] handleStatus);

	DocumentOutProcess findFirstByActiveAndDocIdAndHandleStatusInOrderByIdDesc(boolean active, Long docId,
			DocumentOutHandleStatusEnum[] enums);

	@Query("Select d from DocumentOutProcess d where (:active is null OR d.active = :active) AND d.docId = :docId AND d.userId = :userId AND d.handleStatus IN (:handleStatus) ORDER BY d.id DESC")
	List<DocumentOutProcess> getByDocIdAndUserIdAndHandleStatus(Boolean active, Long docId, Long userId,
			DocumentOutHandleStatusEnum[] handleStatus);

	@Modifying()
	@Query("UPDATE DocumentOutProcess d SET d.active = :active WHERE d.active = true AND d.docId = :docId AND d.userId = :userId AND d.handleStatus IN (:handleStatus)")
	void setActiveByDocIdAndUserIdAndhandleStatus(Boolean active, Long docId, Long userId,
			DocumentOutHandleStatusEnum[] handleStatus);

	@Query("SELECT new com.vz.backend.business.dto.DocumentProcessDto( du.important, p.id,  "
			+ " p.handler.id, p.handler.userName, p.handler.fullName, p.updateDate, p.documentOut, "
			+ " p.handleStatus,  u1.id, u1.userName, u1.fullName, u2.id, u2.userName, u2.fullName, p.read) "
			+ "FROM DocumentOutProcess p "
			+ "LEFT JOIN DocumentUser du ON du.docId = p.docId  AND du.userId in (:userIds) AND du.docType = :docType "
			+ "LEFT JOIN Delegate d1 ON d1.id = p.delegateId AND d1.startDate <= :date AND d1.endDate >= :date "
			+ "LEFT JOIN User u2 ON u2.id = d1.toUserId " + "LEFT JOIN User u1 ON u1.id = p.delegateUserId "
			+ "WHERE (p.docId, coalesce(p.updateDate, p.createDate)) "
			+ "IN (SELECT t.docId, max(coalesce(t.updateDate, t.createDate)) from DocumentOutProcess t "
			+ "WHERE (t.userId  in (:userIds) OR d1.toUserId in (:userIds)) AND t.active=TRUE group by t.docId) "
			+ "AND p.active=TRUE AND (p.userId in (:userIds) OR d1.toUserId in (:userIds)) AND p.handleStatus IN :status AND p.documentOut.status IN (:docStatus) "
			+ "AND p.docId NOT IN (SELECT p1.docId FROM DocumentOutProcess p1 WHERE p1.handlerId = p.delegateUserId AND p1.delegateId != NULL AND p1.active =TRUE AND p1.handleStatus IN (:notYetMainStatus) ) "
			+ "AND (coalesce(:docNoComment, null) IS NULL OR p.docId NOT IN :docNoComment) "
			+ "AND (coalesce(:docComment, null) IS NULL OR p.docId IN :docComment)"
			)
	Page<DocumentProcessDto> findByUserIdAndHandleStatusAndDocStatus(DocumentTypeEnum docType,
																	 List<Long> userIds, DocumentOutHandleStatusEnum[] status,
			DocumentStatusEnum[] docStatus, Date date, DocumentOutHandleStatusEnum[] notYetMainStatus, List<Long> docComment, List<Long> docNoComment,Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.DocumentProcessDto( du.important, p.id,  "
			+ " p.handler.id, p.handler.userName, p.handler.fullName, p.updateDate, p.documentOut, "
			+ " p.handleStatus,  u1.id, u1.userName, u1.fullName, u2.id, u2.userName, u2.fullName,  p.read) "
			+ "FROM DocumentOutProcess p "
			+ "LEFT JOIN DocumentUser du ON du.docId = p.docId  AND du.userId in (:userIds) AND du.docType = :docType "
			+ "LEFT JOIN User u1 ON u1.id = p.delegateUserId "
			+ "LEFT JOIN Delegate d1 ON d1.id = p.delegateId AND d1.startDate <= :date AND d1.endDate >= :date "
			+ "LEFT JOIN User u2 ON u2.id = d1.toUserId " + "INNER JOIN DocumentOut d ON p.handleStatus IN :status AND "
			+ "p.clientId=:clientId AND p.docId = d.id AND d.active = TRUE AND p.active = TRUE AND d.status IN (:docStatus) "
			+ "WHERE p.userId in (:userIds) AND (p.docId, coalesce(p.updateDate, p.createDate)) IN "
			+ "(SELECT t.docId, max(coalesce(t.updateDate, t.createDate)) from DocumentOutProcess t "
			+ "WHERE t.userId in (:userIds) AND t.active=TRUE group by t.docId)"
			+ "AND (lower(d.preview) LIKE %:text% OR lower(d.numberOrSign) like %:text% OR lower(d.userEnter.userName) like %:text%)"
			+ "AND p.docId NOT IN (SELECT p1.docId FROM DocumentOutProcess p1 WHERE p1.handlerId = p.delegateUserId AND p1.delegateId != NULL AND p1.active =TRUE AND p1.handleStatus IN (:notYetMainStatus) ) "
			)
	Page<DocumentProcessDto> search(DocumentTypeEnum docType, DocumentOutHandleStatusEnum[] status, String text,
			List<Long> userIds, DocumentStatusEnum[] docStatus, Date date, DocumentOutHandleStatusEnum[] notYetMainStatus,
			Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.DocumentProcessDto( du.important, p.id,"
			+ " p.handler.id, p.handler.userName, p.handler.fullName, p.updateDate, p.documentOut, "
			+ " p.handleStatus,  u1.id, u1.userName, u1.fullName, u2.id, u2.userName, u2.fullName,  p.read) "
			+ "FROM DocumentOutProcess p "
			+ "LEFT JOIN DocumentUser du ON du.docId = p.docId  AND du.userId in (:userIds) AND du.docType = :docType "
			+ "LEFT JOIN User u1 ON u1.id = p.delegateUserId "
			+ "LEFT JOIN Delegate d1 ON d1.id = p.delegateId AND d1.startDate <= :date AND d1.endDate >= :date "
			+ "LEFT JOIN User u2 ON u2.id = d1.toUserId " + "INNER JOIN DocumentOut d ON p.handleStatus IN :status AND "
			+ "p.clientId=:clientId AND p.docId = d.id AND d.active = TRUE AND p.active = TRUE AND d.status IN (:docStatus) "
			+ "INNER JOIN User u ON d.personEnterId=u.id " + "WHERE p.userId in (:userIds) "
			+ "AND (p.docId, coalesce(p.updateDate, p.createDate)) in "
			+ "(SELECT t.docId, max(coalesce(t.updateDate, t.createDate)) from DocumentOutProcess t "
			+ "WHERE t.userId in (:userIds) AND t.active=TRUE group by t.docId) "
			+ "AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
			+ "AND (:preview IS NULL OR lower(d.preview) LIKE %:preview%) "
			+ "AND (:numberOrSign IS NULL OR lower(d.numberOrSign) LIKE %:numberOrSign%) "
			+ "AND (:orgName IS NULL OR lower(p.orgName) LIKE %:orgName%) "
			+ "AND (:docTypeId IS NULL OR d.docTypeId = :docTypeId) "
			+ "AND (:docFieldId IS NULL OR d.docFieldId = :docFieldId) "
			+ "AND (:personEnter IS NULL OR LOWER(u.userName) = :personEnter OR LOWER(u.fullName) LIKE '%'||:personEnter||'%') "
			+ "AND (coalesce(:startDate, null) IS NULL OR d.createDate > :startDate) "
			+ "AND (coalesce(:endDate, null) IS NULL OR d.createDate < :endDate) "
			+ "AND p.docId NOT IN (SELECT p1.docId FROM DocumentOutProcess p1 WHERE p1.handlerId = p.delegateUserId AND p1.delegateId != NULL AND p1.active =TRUE AND p1.handleStatus IN (:notYetMainStatus) ) ")
	Page<DocumentProcessDto> searchAdvance(DocumentTypeEnum docType, Boolean important, Long clientId,
			DocumentOutHandleStatusEnum[] status, String preview,
			String numberOrSign, Long docTypeId,
			Long docFieldId, String orgName,
			String personEnter, List<Long> userIds, Date startDate,
			Date endDate, DocumentStatusEnum[] docStatus, Date date, DocumentOutHandleStatusEnum[] notYetMainStatus, Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.DocumentProcessDto( du.important, p.id, "
			+ " p.handler.id, p.handler.userName, p.handler.fullName, p.updateDate, p.documentOut, "
			+ " p.handleStatus,  u1.id, u1.userName, u1.fullName, u2.id, u2.userName, u2.fullName,  p.read) "
			+ "FROM DocumentOutProcess p "
			+ "LEFT JOIN DocumentUser du ON du.docId = p.docId  AND du.userId in (:userIds) AND du.docType = :docType "
			+ "LEFT JOIN User u1 ON u1.id = p.delegateUserId "
			+ "LEFT JOIN Delegate d1 ON d1.id = p.delegateId AND d1.startDate <= :date AND d1.endDate >= :date "
			+ "LEFT JOIN User u2 ON u2.id = d1.toUserId " + "INNER JOIN DocumentOut d ON p.handleStatus IN :status AND "
			+ "p.clientId=:clientId AND p.docId = d.id AND d.active = TRUE AND p.active = TRUE AND d.status IN (:docStatus) "
			+ "INNER JOIN User u ON d.personEnterId=u.id " + "WHERE p.userId in (:userIds) "
			+ "AND (p.docId, coalesce(p.updateDate, p.createDate)) in "
			+ "(SELECT t.docId, max(coalesce(t.updateDate, t.createDate)) from DocumentOutProcess t "
			+ "WHERE t.userId in (:userIds) AND t.active=TRUE group by t.docId) "
			+ "AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
			+ "AND (:preview IS NULL OR lower(d.preview) LIKE %:preview%) "
			+ "AND (:numberOrSign IS NULL OR lower(d.numberOrSign) LIKE %:numberOrSign%) "
			+ "AND (:orgName IS NULL OR lower(p.orgName) LIKE %:orgName%) "
			+ "AND (:docTypeId IS NULL OR d.docTypeId = :docTypeId) "
			+ "AND (:docFieldId IS NULL OR d.docFieldId = :docFieldId) "
			+ "AND (:personEnter IS NULL OR LOWER(u.userName) = :personEnter OR LOWER(u.fullName) LIKE '%'||:personEnter||'%') "
			+ "AND (coalesce(:startDate, null) IS NULL OR d.createDate > :startDate) "
			+ "AND (coalesce(:endDate, null) IS NULL OR d.createDate < :endDate) "
			+ "AND p.docId NOT IN (SELECT p1.docId FROM DocumentOutProcess p1 WHERE p1.handlerId = p.delegateUserId AND p1.delegateId != NULL AND p1.active =TRUE AND p1.handleStatus IN (:notYetMainStatus) ) "
			+ "ORDER BY (CASE WHEN du.important is TRUE THEN 1 ELSE 2 END) ASC")
	Page<DocumentProcessDto> searchAdvanceWithImportant(DocumentTypeEnum docType, Boolean important, Long clientId,
			DocumentOutHandleStatusEnum[] status, String preview,
			String numberOrSign, Long docTypeId,
			Long docFieldId, String orgName,
			String personEnter, List<Long> userIds, Date startDate,
			Date endDate, DocumentStatusEnum[] docStatus, Date date, DocumentOutHandleStatusEnum[] notYetMainStatus, Pageable pageable);

	@Query("Select d from DocumentOutProcess d where d.docId = :docId AND (:clientId is null or d.clientId = :clientId) ORDER BY d.id DESC")
	List<DocumentOutProcess> findByDocIdAndClientId(Long docId, Long clientId);

	@Query("select count(distinct p.docId) from DocumentOutProcess p where " + "p.active = true AND p.userId = :userId "
			+ "and p.clientId = :clientId " + "and p.handleStatus IN :status")
	Long countDocByUser(Long clientId, Long userId, DocumentOutHandleStatusEnum[] status);

	@Query("Select d from DocumentOutProcess d " + " where d.docId in (:docId) " + " AND d.clientId = :clientId "
			+ " AND d.active  = true"
			+ " AND (d.docId, coalesce(d.updateDate, d.createDate)) in (SELECT sd.docId, max(coalesce(sd.updateDate, sd.createDate)) from DocumentOutProcess sd WHERE sd.clientId = :clientId AND sd.docId in (:docId) and sd.active=TRUE group by sd.docId)"
			+ " ORDER BY d.id DESC")
	List<DocumentOutProcess> findByDocIdAndClientIdLasted(List<Long> docId, Long clientId);

	@Query("SELECT DISTINCT d.userId FROM DocumentOutProcess d WHERE d.docId = :docId AND (:active is null OR d.active = :active) AND d.clientId = :clientId")
	List<Long> getListUserIdByDocIdAndActiveAndClientId(long docId, Boolean active, long clientId);

	@Query("SELECT count(*)>0 FROM DocumentOutProcess d LEFT JOIN Delegate d1 ON d1.id = d.delegateId AND d1.startDate <= :date AND d1.endDate >= :date "
			+ "WHERE d.docId = :docId AND (d.userId in :userId OR d1.toUserId in :userId) AND (:active is null OR d.active = :active) AND d.clientId = :clientId")
	boolean existUserInProcessByDocIdAndActiveAndClientId(List<Long> userId, Long docId, boolean active, Long clientId, Date date);

	@Query("SELECT count(*)>0 FROM DocumentOutProcess d WHERE d.docId = :docId AND d.userId IN (:listUser) AND (:active is null OR d.active = :active) AND d.clientId = :clientId")
	boolean existListUserInProcessByDocIdAndActive(List<Long> listUser, Long docId, boolean active, Long clientId);

	@Query("Select d from DocumentOutProcess d " + " where d.docId in (:docId) " + " AND d.clientId = :clientId "
			+ " AND d.active  = true"
			+ " AND (d.docId, d.userId, coalesce(d.updateDate, d.createDate)) in (SELECT sd.docId, sd.userId, max(coalesce(sd.updateDate, sd.createDate)) from DocumentOutProcess sd WHERE sd.clientId = :clientId AND sd.docId in (:docId) and sd.active=TRUE group by sd.docId, sd.userId)"
			+ " ORDER BY d.id DESC, d.userId DESC")
	List<DocumentOutProcess> findByDocIdAndClientIdLastedAndUser(List<Long> docId, Long clientId);

	List<DocumentOutProcess> findByDocIdInAndUserIdOrderByIdDesc(List<Long> idList, Long userId);

	@Query("SELECT new com.vz.backend.business.dto.fullreport.SimpleProcessOut(p.id, p.docId, p.updateDate, p.handleStatus) FROM DocumentOutProcess p "
			+ "WHERE p.active is TRUE AND p.userId = :userId AND p.clientId = :clientId AND "
			+ "(coalesce(:startDate, null) is NULL or p.updateDate BETWEEN :startDate and :endDate)")
	List<SimpleProcessOut> fullReport(Long userId, Long clientId, Date startDate, Date endDate);

	@Query("SELECT p.createBy FROM DocumentOutProcess p WHERE p.userId = :userId AND p.docId = :docId AND p.handleStatus IN (:handleStatus) AND p.clientId = :clientId AND p.active is true")
	List<Long> findUserXYKByUserIdAndDocIdAndHandleStatusIn(Long userId, Long docId, List<DocumentOutHandleStatusEnum> handleStatus, Long clientId);

	@Query("SELECT p.userId FROM DocumentOutProcess p WHERE p.createBy = :userId AND p.docId = :docId AND p.handleStatus IN (:handleStatus) AND p.clientId = :clientId AND p.active is true")
	List<Long> findUserYKByUserIdAndDocIdAndHandleStatusIn(Long userId, Long docId, List<DocumentOutHandleStatusEnum> handleStatus, Long clientId);

	@Query("SELECT COUNT(*)>0 FROM DocumentOutProcess p WHERE p.docId=:docId")
	boolean isNoProcess(Long docId);

	@Query("SELECT new com.vz.backend.business.dto.DocumentOutProcessDto(p.docId, p.userId, p.nodeId, p.user.fullName, p.user.positionModel.name, p.user.orgModel.name ) FROM DocumentOutProcess p "
			+ " WHERE p.docId=:docId AND p.clientId=:clientId AND p.active=TRUE AND p.userId IN (:userTransfers) AND p.handleStatus in (:status)"
			+ " GROUP BY p.docId, p.userId, p.nodeId, p.user.fullName, p.user.positionModel.name, p.user.orgModel.name ")
	List<DocumentOutProcessDto> findListUserTransfer(Long docId, Long clientId,
			List<Long> userTransfers, DocumentOutHandleStatusEnum[] status);

	DocumentOutProcess findFirstByActiveAndDocIdAndUserIdAndHandleStatusInOrderByIdDesc(boolean active, Long docId,
			Long userId, DocumentOutHandleStatusEnum[] enums);

	@Query("SELECT distinct p.userId FROM DocumentOutProcess p "
			+ "WHERE p.clientId=:clientId AND p.docId=:docId")
	List<Long> getAllRelate(Long docId, Long clientId);

	@Query("SELECT p FROM DocumentOutProcess p "
			+ "LEFT JOIN Delegate d1 ON d1.id = p.delegateId AND d1.startDate <= current_date() AND d1.endDate >= current_date() "
			+ "WHERE p.clientId=:clientId AND p.docId=:docId AND (p.userId in :userId OR d1.toUserId in :userId) AND p.handleStatus IN (:enums) AND p.active = TRUE ORDER BY p.id DESC")
	List<DocumentOutProcess> findByUserRelatedAndDocId(Long docId, List<Long> userId, DocumentOutHandleStatusEnum[] enums, Long clientId);

	@Query("SELECT p FROM DocumentOutProcess p "
			+ "LEFT JOIN Delegate d1 ON d1.id = p.delegateId AND d1.startDate <= current_date() AND d1.endDate >= current_date() "
			+ "WHERE p.clientId=:clientId AND p.docId=:docId AND (p.userId in :userId OR d1.toUserId in :userId) AND p.active = TRUE ORDER BY p.id DESC")
	List<DocumentOutProcess> findByUserRelatedAndDocId(Long docId, List<Long> userId, Long clientId);

	@Query("SELECT p FROM DocumentOutProcess p "
			+ "WHERE p.clientId=:clientId AND p.docId=:docId AND (p.delegateUserId =:userId) AND (:status IS NULL OR p.handleStatus = :status) AND (:active IS NULL OR p.active = :active) ORDER BY p.id DESC")
	List<DocumentOutProcess> findDelegate(Long docId, Long userId, DocumentOutHandleStatusEnum status,
			Long clientId, Boolean active);

	@Query("SELECT p FROM DocumentOutProcess p "
			+ "WHERE p.clientId=:clientId AND p.docId=:docId AND p.userId =:userId AND p.delegateUserId != null AND p.active = TRUE ORDER BY p.id DESC")
	List<DocumentOutProcess> findDelegate(Long docId, Long userId, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.kpi.KPIDataDto(p) FROM DocumentOutProcess p "
			+ " WHERE (p.userId IN (:userIds) OR p.delegateId IN (:userIds)) AND p.clientId =:clientId AND p.active = TRUE AND p.documentOut.active = TRUE"
			+ " AND (COALESCE(:startDate, NULL) IS NULL OR COALESCE(:endDate, NULL) IS NULL OR COALESCE(p.updateDate, p.createDate) IS NULL OR p.updateDate BETWEEN :startDate AND :endDate)")
	List<KPIDataDto> findAllByToUser(List<Long> userIds, Long clientId, Date startDate, Date endDate);

	@Query("SELECT new com.vz.backend.business.dto.kpi.KPIDataDto(p) FROM DocumentOut d "
			+ " LEFT JOIN DocumentOutProcess p ON p.docId = d.id AND p.userId = :userId AND p.active IS TRUE "
			+ " LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = 'VAN_BAN_DI'"
			+ " WHERE (p.userId = :userId OR p.documentOut.personEnterId = :userId) AND p.clientId =:clientId AND p.active = TRUE AND p.documentOut.active = TRUE"
			+ " AND (p.docId, COALESCE(p.updateDate, p.createDate)) IN "
			+ " (SELECT t.docId, MAX(COALESCE(t.updateDate, t.createDate)) FROM DocumentOutProcess t WHERE t.userId = :userId AND t.active=TRUE GROUP BY t.docId)"
			+ " AND (COALESCE(:startDate, NULL) IS NULL OR COALESCE(:endDate, NULL) IS NULL OR COALESCE(p.updateDate, p.createDate) IS NULL OR p.updateDate BETWEEN :startDate AND :endDate)"
			+ " AND p.docId NOT IN (SELECT p1.docId FROM DocumentOutProcess p1 WHERE p1.handlerId = p.delegateUserId AND p1.delegateId != NULL AND p1.active =TRUE AND p1.handleStatus IN ('CHO_XU_LY', 'BI_TRA_LAI')) ")
	List<KPIDataDto> findAllByToUser(Long userId, Long clientId, Date startDate, Date endDate);

	@Query("SELECT d.id"
			+ " FROM DocumentOut d LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = 'VAN_BAN_DI' "
			+ " LEFT JOIN ObjectRead obr ON obr.objId =  d.id AND obr.userId = :userId AND obr.type = 'VAN_BAN_DI' "
			+ " LEFT JOIN DocumentReceive dr ON dr.docId = d.id AND dr.receiveId = :userId "
			+ " WHERE ((:done IS TRUE AND dr.status = 'DONE') OR (:done IS FALSE AND (dr.status IS NULL OR dr.status = 'NOT_YET')))"
			+ " AND d.clientId = :clientId AND (d.status = 'DA_BAN_HANH') AND (d.id in ("
			+ " SELECT d1.id FROM DocumentOut d1 INNER JOIN DocumentReceive r ON d1.id=r.docId and d1.active=true and r.active=true "
			+ " LEFT JOIN User u ON (r.type ='ORG' OR r.type = 'ALL') AND r.receiveId=u.org AND u.id=:userId AND r.clientId = :clientId AND d1.clientId = :clientId "
			+ " WHERE d1.status = 'DA_BAN_HANH' "
			+ " AND (COALESCE(:startDate, NULL) IS NULL OR COALESCE(:endDate, NULL) IS NULL OR COALESCE(r.updateDate, r.createDate) IS NULL OR r.updateDate BETWEEN :startDate AND :endDate)"
			+ " AND ((r.type ='FORWARD' AND r.receiveId=:userId) OR (r.type ='USER' AND r.receiveId=:userId) OR (r.type ='ORG' AND u.lead is TRUE AND r.receiveId=u.org ) OR (r.type ='ALL' AND r.receiveId=u.org ))) "
			+ " OR d.id IN (:docIds)) "
//			+ "AND (obr IS NULL OR obr.read = FALSE)"
			)
	List<Long> knowable(Long userId, Date startDate, Date endDate, Long clientId, List<Long> docIds, boolean done);

	@Query(name = "DocumentOut.menu", nativeQuery = true)
	List<ReportDocByTypeDto> reportDocByType(Date date, boolean clericalOrg, Long userId, Long orgId, Long clientId);
}
