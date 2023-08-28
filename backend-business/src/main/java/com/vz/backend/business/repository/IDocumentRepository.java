package com.vz.backend.business.repository;

import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.dto.*;
import com.vz.backend.business.dto.document.ResolvedUserDto;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.dto.CategoryInitDto;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.repository.IRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

@Repository
public interface IDocumentRepository extends IRepository<Documents> {

	@Query(value = "select d from Documents d LEFT JOIN ClericalOrg c ON :clericalOrg is true AND c.userId = : userId AND c.active is true "
			+ "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = :docType "
			+ " where (:text is null or lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text%) "
			+ " AND (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign%) "
			+ " AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
			+ " AND (:expired is null OR (:expired is true AND d.deadline != null AND (:now > d.deadline)) OR (:expired is false AND (d.deadline is null OR (:now <= d.deadline)))) "
			+ " and (:preview is null or lower(d.preview) like %:preview%) "
			+ " and (:docTypeId is null or d.docTypeId = :docTypeId) "
			+ " and (:docFieldsId is null or d.docFieldsId = :docFieldsId) "
			+ " and (:statusReceiptId is null or d.statusReceiptId = :statusReceiptId) "
			+ " and (:clientId is null or d.clientId = :clientId) and d.active = true "
			+ " and ((:docStatusIds) is null or d.status in (:docStatusIds)) "
			+ " AND (d.mergedLines IS NULL OR d.mergedLines IS FALSE) "
			+ " AND ((:clericalOrg is true AND d.orgReceiveId = c.orgId) OR (:clericalOrg is false AND :userList != null AND d.personEnterId IN (:userList))) ")
	Page<Documents> findReceiveAdvance(String text, boolean clericalOrg, Boolean expired, Date now, Boolean important,
			DocumentTypeEnum docType, Long userId, List<Long> userList, String numberOrSign, String preview,
			Long docTypeId, Long docFieldsId, List<DocumentStatusEnum> docStatusIds, Long statusReceiptId,
			Long clientId, Pageable pageable);

	@Query("SELECT NEW com.vz.backend.business.dto.DocumentDto(d, p, ob.read, du.important, do)"
            + " FROM Documents d JOIN DocumentInProcess p ON p.docId = d.id AND p.active = TRUE "
            + " LEFT JOIN NodeModel2 node on node.id = p.node "
            + " LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId in (:userIds) AND du.docType = :docType "
            + " LEFT JOIN ObjectRead ob ON ob.objId = d.id AND ob.userId in (:userIds) AND ob.type = 'VAN_BAN_DEN' "
            + " LEFT JOIN Condition con ON node.id = con.nodeId " +
            " LEFT JOIN DocumentOut do on d.docOutId = do.id and do.active is true"
            + " WHERE (:numberOrSign IS NULL OR LOWER(d.numberOrSign) LIKE %:numberOrSign%) "
            + " AND ((:mergedLines IS NULL AND (d.mergedLines IS NULL OR d.mergedLines IS FALSE)) OR (d.mergedLines = :mergedLines)) "
            + " AND (:preview IS NULL OR LOWER(d.preview) LIKE %:preview% )  "
            + " AND (:text IS NULL OR LOWER(d.numberOrSign) LIKE %:text% OR LOWER(d.preview) LIKE %:text%) "
            + " AND (:dayLeft IS NULL"
            + "		OR :dayLeft = "
            + "				CASE WHEN (COALESCE(p.deadline, NULL) IS NOT NULL AND (p.deadline < current_date())) THEN -1 "
            + "					 WHEN (COALESCE(p.deadline, NULL) IS NULL OR p.deadline > current_date() + 2)  THEN 3 "
            + "					 ELSE 0 END)"
            + " AND (:endTask IS NULL"
            + "		OR :endTask = "
            + "				CASE WHEN d.status IN ('DOING', 'RETURN_DOC') AND p.endTask IS TRUE THEN 2 "
            + "					 WHEN d.status = 'DONE' THEN 2 "
            + "					 WHEN d.status IN ('DOING', 'RETURN_DOC') AND p.endTask IS NULL THEN 0 "
            + "					 ELSE NULL END ) "
            + " AND (:docTypeId IS NULL OR d.docTypeId = :docTypeId) "
            + " AND (:docFieldsId IS NULL OR d.docFieldsId = :docFieldsId) "
            + " AND (d.clientId = :clientId AND p.clientId = :clientId AND d.active = true AND p.active = TRUE) "
            + " AND ((:docStatusIds) IS NULL OR d.status in (:docStatusIds)) "
            + " AND ((:userIds) IS NULL OR p.toUser in (:userIds) OR p.delegaterId in (:userIds)) "
            + " AND p.handleStatus IN (:handleStatus)"
            + " AND ((:handleType) IS NULL OR  p.handleType IN (:handleType)) "
            + " AND (:important IS NULL OR  (:important = FALSE AND du.important IS NULL) OR du.important = :important) "
            + " AND (:expired IS NULL OR (:expired IS TRUE AND d.deadline != NULL AND (:now > d.deadline)) OR (:expired IS FALSE AND (d.deadline IS NULL OR (:now <= d.deadline)))) "
//			+ " AND (d.id, p.step) IN (SELECT sp.docId, MAX(sp.step) FROM DocumentInProcess sp WHERE sp.active = TRUE AND sp.clientId = :clientId "
//			+ "		AND (sp.toUser = :userId OR sp.delegaterId = :userId) "
//			+ "		GROUP BY sp.docId)"
            + " AND p.id IN (:pId) AND (:posId IS NULL or con.positionId = :posId)"
            + " GROUP BY d, p, ob.read, du.important, do"
			)
	Page<DocumentDto> findByMultiConditions(Integer endTask, Boolean mergedLines, String text, Integer dayLeft, Boolean expired, Date now, Boolean important,
			DocumentTypeEnum docType, List<Long> userIds, List<DocumentStatusEnum> docStatusIds,
			List<DocumentInHandleStatusEnum> handleStatus, List<HandleTypeEnum> handleType, String numberOrSign,
			String preview, Long docTypeId, Long docFieldsId, List<Long> pId, Long clientId, Long posId, Pageable pageable);


	@Query("select new com.vz.backend.business.dto.document.DocumentDto(d, case " +
			"when exists (select 1 from DocumentInProcess dip1 where dip1.docId = d.id and dip1.handleStatus = 'DA_XU_LY' and dip1.toUsers.org = :orgId) " +
			"then 'HANDLED' " +
			"when not exists (select 1 from DocumentInProcess dip2 where dip2.docId = d.id and dip2.handleStatus = 'DA_XU_LY' and dip2.toUsers.org = :orgId) " +
			"and exists (select 1 from DocumentInProcess dip3 where dip3.docId = d.id and dip3.toUsers.org = :orgId and dip3.deadline is not null and date(dip3.deadline) < date(:currentDate)) " +
			"then 'OVERDUE' " +
			"when not exists (select 1 from DocumentInProcess dip4 where dip4.docId = d.id and dip4.handleStatus = 'DA_XU_LY' and dip4.toUsers.org = :orgId) " +
			"and exists (select 1 from DocumentInProcess dip5 where dip5.docId = d.id and dip5.toUsers.org = :orgId and dip5.deadline is not null and date(dip5.deadline) = date(:currentDate)) " +
			"then 'DUE' " +
			"when not exists (select 1 from DocumentInProcess dip6 where dip6.docId = d.id and dip6.handleStatus = 'DA_XU_LY' and dip6.toUsers.org = :orgId) " +
			"and exists (select 1 from DocumentInProcess dip7 where dip7.docId = d.id and dip7.handleStatus = 'DANG_XU_LY' and dip7.toUsers.org = :orgId) " +
			"and exists (select 1 from DocumentInProcess dip8 where dip8.docId = d.id and dip8.toUsers.org = :orgId and (dip8.deadline is not null and date(dip8.deadline) > date(:currentDate)) or dip8.deadline is null) " +
			"then 'SEEN' " +
			"else 'NEW' " +
			"end, (select min(dp.deadline) from DocumentInProcess dp where dp.docId = d.id and dp.toUsers.org = :orgId) ) from Documents d " +
			"right join DocumentInProcess p on p.docId = d.id and p.active = true " +
			"where p.toUsers.org = :orgId " +
			"and (coalesce(:startDate, null) is null or d.dateIssued >= :startDate) " +
			"and (coalesce(:endDate, null) is null or d.dateIssued <= :endDate) and d.clientId = :clientId and p.handleType = :handleTypeEnum " +
			"group by d.id, p.toUsers.org")
	List<com.vz.backend.business.dto.document.DocumentDto> getDocumentHandleStateByOrgId(HandleTypeEnum handleTypeEnum, Long orgId, Date currentDate, Date startDate, Date endDate, Long clientId);

	@Query(
			value = "select new com.vz.backend.business.dto.document.DocumentDto(d, case " +
					"when exists (select 1 from DocumentInProcess dip1 where dip1.docId = d.id and dip1.handleStatus = 'DA_XU_LY' and dip1.toUsers.org = :orgId) " +
					"then 'HANDLED' " +
					"when not exists (select 1 from DocumentInProcess dip2 where dip2.docId = d.id and dip2.handleStatus = 'DA_XU_LY' and dip2.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip3 where dip3.docId = d.id and dip3.toUsers.org = :orgId and dip3.deadline is not null and date(dip3.deadline) < date(:currentDate)) " +
					"then 'OVERDUE' " +
					"when not exists (select 1 from DocumentInProcess dip4 where dip4.docId = d.id and dip4.handleStatus = 'DA_XU_LY' and dip4.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip5 where dip5.docId = d.id and dip5.toUsers.org = :orgId and dip5.deadline is not null and date(dip5.deadline) = date(:currentDate)) " +
					"then 'DUE' " +
					"when not exists (select 1 from DocumentInProcess dip6 where dip6.docId = d.id and dip6.handleStatus = 'DA_XU_LY' and dip6.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip7 where dip7.docId = d.id and dip7.handleStatus = 'DANG_XU_LY' and dip7.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip8 where dip8.docId = d.id and dip8.toUsers.org = :orgId and (dip8.deadline is not null and date(dip8.deadline) > date(:currentDate)) or dip8.deadline is null) " +
					"then 'SEEN' " +
					"else 'NEW' " +
					"end, (select min(dp.deadline) from DocumentInProcess dp where dp.docId = d.id and dp.toUsers.org = :orgId)) from Documents d " +
					"right join DocumentInProcess p on p.docId = d.id and p.active = true " +
					"where p.toUsers.org = :orgId " +
					"and (coalesce(:startDate, null) is null or d.dateIssued >= :startDate) " +
					"and (coalesce(:endDate, null) is null or d.dateIssued <= :endDate) and d.clientId = :clientId and p.handleType = :handleTypeEnum " +

					" and case " +
					"when exists (select 1 from DocumentInProcess dip1 where dip1.docId = d.id and dip1.handleStatus = 'DA_XU_LY' and dip1.toUsers.org = :orgId) " +
					"then 'HANDLED' " +
					"when not exists (select 1 from DocumentInProcess dip2 where dip2.docId = d.id and dip2.handleStatus = 'DA_XU_LY' and dip2.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip3 where dip3.docId = d.id and dip3.toUsers.org = :orgId and dip3.deadline is not null and date(dip3.deadline) < date(:currentDate)) " +
					"then 'OVERDUE' " +
					"when not exists (select 1 from DocumentInProcess dip4 where dip4.docId = d.id and dip4.handleStatus = 'DA_XU_LY' and dip4.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip5 where dip5.docId = d.id and dip5.toUsers.org = :orgId and dip5.deadline is not null and date(dip5.deadline) = date(:currentDate)) " +
					"then 'DUE' " +
					"when not exists (select 1 from DocumentInProcess dip6 where dip6.docId = d.id and dip6.handleStatus = 'DA_XU_LY' and dip6.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip7 where dip7.docId = d.id and dip7.handleStatus = 'DANG_XU_LY' and dip7.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip8 where dip8.docId = d.id and dip8.toUsers.org = :orgId and (dip8.deadline is not null and date(dip8.deadline) > date(:currentDate)) or dip8.deadline is null) " +
					"then 'SEEN' " +
					"else 'NEW' " +
					"end = :viewStatusEnum  " +
					"group by d.id, p.toUsers.org",
			countQuery = "select count(d.id) from Documents d " +
					"right join DocumentInProcess p on p.docId = d.id and p.active = true " +
					"where p.toUsers.org = :orgId " +
					"and (coalesce(:startDate, null) is null or d.dateIssued >= :startDate) " +
					"and (coalesce(:endDate, null) is null or d.dateIssued <= :endDate) and d.clientId = :clientId and p.handleType = :handleTypeEnum " +
					" and case " +
					"when exists (select 1 from DocumentInProcess dip1 where dip1.docId = d.id and dip1.handleStatus = 'DA_XU_LY' and dip1.toUsers.org = :orgId) " +
					"then 'HANDLED' " +
					"when not exists (select 1 from DocumentInProcess dip2 where dip2.docId = d.id and dip2.handleStatus = 'DA_XU_LY' and dip2.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip3 where dip3.docId = d.id and dip3.toUsers.org = :orgId and dip3.deadline is not null and date(dip3.deadline) < date(:currentDate)) " +
					"then 'OVERDUE' " +
					"when not exists (select 1 from DocumentInProcess dip4 where dip4.docId = d.id and dip4.handleStatus = 'DA_XU_LY' and dip4.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip5 where dip5.docId = d.id and dip5.toUsers.org = :orgId and dip5.deadline is not null and date(dip5.deadline) = date(:currentDate)) " +
					"then 'DUE' " +
					"when not exists (select 1 from DocumentInProcess dip6 where dip6.docId = d.id and dip6.handleStatus = 'DA_XU_LY' and dip6.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip7 where dip7.docId = d.id and dip7.handleStatus = 'DANG_XU_LY' and dip7.toUsers.org = :orgId) " +
					"and exists (select 1 from DocumentInProcess dip8 where dip8.docId = d.id and dip8.toUsers.org = :orgId and (dip8.deadline is not null and date(dip8.deadline) > date(:currentDate)) or dip8.deadline is null) " +
					"then 'SEEN' " +
					"else 'NEW' " +
					"end = :viewStatusEnum  " +
					"group by d.id, p.toUsers.org"
	)
	Page<com.vz.backend.business.dto.document.DocumentDto> pageDocumentDtoByHandleState(String viewStatusEnum, HandleTypeEnum handleTypeEnum, Long orgId, Date currentDate, Date startDate, Date endDate, Long clientId, Pageable pageable);

	@Query(name = "Documents.step", nativeQuery = true)
	List<KnowableDto> getPIdByMaxStep(List<Long> userIds, Long clientId);

	@Query("select new com.vz.backend.core.dto.CategoryInitDto(db.id , concat(db.name, ' - ', CAST (db.year AS text)), max(d.numberArrival), db.numberOrSign, db.year) "
			+ " from Documents d right join DocumentBook db on db.id = d.bookId and db.active = true and d.active = true and d.clientId = :clientId and db.clientId = :clientId"
			+ " where db.bookType = 0  and db.clientId = :clientId and db.active = true and (d is null or d.active = true) "
			+ " group by db.id , db.name ")
	List<CategoryInitDto> getMaxNumberArrivalByClientId(Long clientId);

	@Query("select new com.vz.backend.core.dto.CategoryInitDto(db.id , concat(db.name, ' - ', CAST (db.year AS text)), max(d.numberArrival), db.numberOrSign, db.year) "
			+ " from Documents d right join DocumentBook db on db.id = d.bookId and db.active = true and d.active = true and d.clientId = :clientId and db.clientId = :clientId"
			+ " WHERE db.id IN (SELECT odb.bookId FROM OrgDocBook odb WHERE odb.active = true AND odb.orgId IN :orgIds) "
			+ "AND db.bookType = 0  and db.clientId = :clientId and db.active = true and (d is null or d.active = true) "
			+ " group by db.id , db.name ORDER BY db.name")
	List<CategoryInitDto> getMaxNumberArrivalByOrgIdAndClientId(List<Long> orgIds, Long clientId);

	List<Documents> findByClientIdAndNumberArrivalAndBookIdAndActiveTrue(Long clientId, Long numberArrival, Long bookId);

	@Query(value = "select new com.vz.backend.business.dto.DocumentReplyDto(d.numberArrival, d.numberOrSign, d.dateArrival, d.dateIssued, d.preview, d.placeSend, d.deadline, d.id) "
			+ " from Documents d where (d.id in (:ids)) and (:clientId is null or d.clientId = :clientId )"
			+ " and d.active = true order by d.createDate desc")
	List<DocumentReplyDto> findDocReplyByIds(Long[] ids,
			Long clientId);

	@Query(value = "select d from Documents d "
			+ " where (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign%) "
			+ " and (:preview is null or lower(d.preview) like %:preview%) "
			+ " and (:docStatusId is null or d.status = :docStatusId) "
			+ " and (coalesce(:startArrival, null) is null or d.dateArrival > :startArrival) "
			+ " and (coalesce(:endArrival, null) is null or d.dateArrival < :endArrival) "
			+ " and (coalesce(:startIssued, null) is null or d.dateIssued > :startIssued) "
			+ " and (coalesce(:endIssued, null) is null or d.dateIssued < :endIssued) "
			+ " and (:clientId is null or d.clientId = :clientId )"
			+ " and (:orgExe is null or lower(d.placeSend) like %:orgExe%) and d.active = true"
			+ " and d.status NOT IN (:hideDocStatus) "
			+ " and (d.id in (select sp.docId from DocumentInProcess sp where sp.clientId = :clientId "
			+ "and sp.toUser = :toUser and sp.handleStatus in (:handleStatusList) group by sp.docId))")
	Page<Documents> findDocReplys(List<DocumentStatusEnum> hideDocStatus,
			List<DocumentInHandleStatusEnum> handleStatusList,
			Long toUser, Long clientId,
			String numberOrSign, String preview,
			String orgExe, DocumentStatusEnum docStatusId,
			Date startArrival, Date endArrival,
			Date startIssued, Date endIssued, Pageable page);

	public static final String FIND_ALL_QUERY =
			"select new com.vz.backend.business.dto.DocumentDto(d, '', du.important) "
			+ " from Documents d left join DocumentInProcess p on p.docId = d.id and p.active = TRUE and p.clientId = d.clientId "
			+ " LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId in (:userId) AND du.docType = 'VAN_BAN_DEN'"
			+ " LEFT JOIN ObjectTag ot ON ot.objId = d.id AND ot.active = TRUE AND ot.type = 'VAN_BAN_DEN' "
			+ " LEFT JOIN Tag t ON t.id = ot.tagId AND t.active = TRUE AND t.createBy in (:userId) "
			+ " where (:text is null or lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text% or lower(t.name) like %:text% )"
			+ " and (d.mergedLines IS NULL OR d.mergedLines IS FALSE) "
			+ " and (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign% or lower(t.name) like %:numberOrSign% ) "
			+ " and (:preview is null or lower(d.preview) like %:preview% or lower(t.name) like %:preview% ) "
			+ " and (:docTypeId is null or d.docTypeId = :docTypeId) "
			+ " and (:docFieldsId is null or d.docFieldsId = :docFieldsId) "
			+ " and (:bookId is null or d.bookId = :bookId) "
			+ " and (:orgIssuedName is null or lower(d.placeSend) like %:orgIssuedName%) "
			+ " and (:docStatusId is null or d.status = :docStatusId) "
			+ " and (:urgentId is null or d.urgentId = :urgentId) "
			+ " and (:securityId is null or d.securityId = :securityId) "
			+ " and ((:numberArrival is null or (concat(d.numberArrival,lower(d.documentBook.numberOrSign))) like  %:numberArrival%)"
			+ " or CAST(d.numberArrival AS string) like %:numberArrival%)"
			+ " and (:personSign is null or lower(d.personSign) like %:personSign%) "
			+ " and (coalesce(:startArrival, null) is null or d.dateArrival > :startArrival) "
			+ " and (coalesce(:endArrival, null) is null or d.dateArrival < :endArrival) "
			+ " and (coalesce(:startIssued, null) is null or d.dateIssued > :startIssued) "
			+ " and (coalesce(:endIssued, null) is null or d.dateIssued < :endIssued) "
			+ " and (coalesce(:startReceived, null) is null or d.receivedDate > :startReceived) "
			+ " and (coalesce(:endReceived, null) is null or d.receivedDate < :endReceived) "
			+ " and (d.clientId = :clientId) and d.active = true "
			+ " AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
			+ " AND (:expired is null OR (:expired is true AND d.deadline != null AND (:now > d.deadline)) OR (:expired is false AND (d.deadline is null OR (:now <= d.deadline)))) "
			+ " and d.status NOT IN('REJECT_RECEIVE', 'WAIT_RECEIVE')"
			+ " and d.id in (:withProcess)"
			+ " and ( (d.id, coalesce(p.updateDate, p.createDate)) in (select sp3.docId, MAX(coalesce(sp3.updateDate, sp3.createDate)) from DocumentInProcess sp3 where sp3.active = TRUE AND sp3.clientId = :clientId and (sp3.toUser in (:userId) or sp3.delegaterId in (:userId)) group by sp3.docId )"
			+ " or d.id in :witDocument"
			+ " ) group by d, du "
			//			+ "ORDER BY (CASE WHEN du.important is TRUE THEN 1 ELSE 2 END) ASC"
			;

	public static final String FIND_ALL_QUERYS =
			"select new com.vz.backend.business.dto.DocumentDto(d, '', du.important) "
					+ " from Documents d left join DocumentInProcess p on p.docId = d.id and p.active = TRUE and p.clientId = d.clientId "
					+ " LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = 'VAN_BAN_DEN'"
					+ " LEFT JOIN ObjectTag ot ON ot.objId = d.id AND ot.active = TRUE AND ot.type = 'VAN_BAN_DEN' "
					+ " LEFT JOIN Tag t ON t.id = ot.tagId AND t.active = TRUE AND t.createBy = :userId "
					+ " and (d.mergedLines IS NULL OR d.mergedLines IS FALSE) "
					+ " and (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign% or lower(t.name) like %:numberOrSign% ) "
					+ " and (:preview is null or lower(d.preview) like %:preview% or lower(t.name) like %:preview% ) "
					+ " and (:docFieldsId is null or d.docFieldsId = :docFieldsId) "
					+ " and (:bookId is null or d.bookId = :bookId) "
					+ " and (:orgIssuedName is null or lower(d.placeSend) like %:orgIssuedName%) "
					+ " and (:docStatusId is null or d.status = :docStatusId) "
					+ " and (:urgentId is null or d.urgentId = :urgentId) "
					+ " and (:securityId is null or d.securityId = :securityId) "
					+ " and ((:numberArrival is null or (concat(d.numberArrival,lower(d.documentBook.numberOrSign))) like  %:numberArrival%)"
					+ " or CAST(d.numberArrival AS string) like %:numberArrival%)"
					+ " and (coalesce(:startArrival, null) is null or d.dateArrival > :startArrival) "
					+ " and (coalesce(:endArrival, null) is null or d.dateArrival < :endArrival) "
					+ " and (coalesce(:startIssued, null) is null or d.dateIssued > :startIssued) "
					+ " and (coalesce(:endIssued, null) is null or d.dateIssued < :endIssued) "
					+ " and (coalesce(:startReceived, null) is null or d.receivedDate > :startReceived) "
					+ " and (coalesce(:endReceived, null) is null or d.receivedDate < :endReceived) "
					+ " and (d.clientId = :clientId) and d.active = true "
					+ " AND ((d.deadline != null AND (:now > d.deadline)) OR ((d.deadline is null OR (:now <= d.deadline)))) "
					+ " and d.status NOT IN('REJECT_RECEIVE', 'WAIT_RECEIVE')"
					+ " and d.id in (:withProcess)"
					+ " and ( (d.id, coalesce(p.updateDate, p.createDate)) in (select sp3.docId, MAX(coalesce(sp3.updateDate, sp3.createDate)) from DocumentInProcess sp3 where sp3.active = TRUE AND sp3.clientId = :clientId and (sp3.toUser = (:userId) or sp3.delegaterId = (:userId)) group by sp3.docId )"
					+ " or d.id in :witDocument"
					+ " ) group by d, du "
			//			+ "ORDER BY (CASE WHEN du.important is TRUE THEN 1 ELSE 2 END) ASC"
			;
	@Query(value = FIND_ALL_QUERY)
	Page<DocumentDto> findAdvanceAll(String text, Boolean expired, Date now, Boolean important, List<Long> userId,
			String numberOrSign, String preview, Long bookId, Long docTypeId, String orgIssuedName, Long docFieldsId,
			DocumentStatusEnum docStatusId, Long urgentId, Long securityId, String numberArrival, Date startArrival,
			Date endArrival, Date startIssued, Date endIssued, Date startReceived, Date endReceived, String personSign,
			Long clientId, List<Long> withProcess, List<Long> witDocument, Pageable pageable);

	@Query("select sd.id from Documents sd "
			+ "LEFT JOIN ClericalOrg c ON :clericalOrg is true AND c.userId = :userId AND c.active is true "
			+ "where sd.clientId = :clientId and sd.active = true "
			+ "and ((:clericalOrg is true AND sd.orgReceiveId = c.orgId) OR (:clericalOrg is false AND sd.personEnterId = (:userId))) "
			+ "and sd.status = 'NOT_YET'")
	List<Long> witDocument(Long clientId, boolean clericalOrg, Long userId);

		@Query(value = FIND_ALL_QUERY)
	List<DocumentDto> findAdvanceAllNotPaging(String text, Boolean expired, Date now, Boolean important, Long userId,
			String numberOrSign, String preview, Long bookId, Long docTypeId, String orgIssuedName, Long docFieldsId,
			DocumentStatusEnum docStatusId, Long urgentId, Long securityId, String numberArrival, Date startArrival,
			Date endArrival, Date startIssued, Date endIssued, Date startReceived, Date endReceived, String personSign,
				Long clientId, List<Long> withProcess, List<Long> witDocument, Sort sort);

	@Query(value = FIND_ALL_QUERYS)
	List<DocumentDto> findAdvanceAllNotPagings(Date now, Long userId,
											  String numberOrSign, String preview, Long bookId, String orgIssuedName, Long docFieldsId,
											  DocumentStatusEnum docStatusId, Long urgentId, Long securityId, String numberArrival, Date startArrival,
											  Date endArrival, Date startIssued, Date endIssued, Date startReceived, Date endReceived,
											  Long clientId, List<Long> withProcess, List<Long> witDocument, Sort sort);

	@Query(value = "select d from Documents d "
			+ " where ((:numberOrSign) is null or lower(d.numberOrSign) like %:numberOrSign%) "
			+ " and ((:preview) is null or lower(d.preview) like %:preview%) "
			+ " and (((:docStatusId) is null or d.status = (:docStatusId)) and d.status is not 'RETAKE_DOC') "
			+ " and ((:urgentId) is null or d.urgentId = (:urgentId)) "
			+ " and ((:securityId) is null or d.securityId = (:securityId)) "
			+ " AND ((:placeSendId) is null or d.placeSendId = (:placeSendId)) "
			+ " AND (coalesce(:dateArrivalFrom, null) is null or d.dateArrival >= :dateArrivalFrom) "
			+ " AND (coalesce(:dateArrivalTo, null) is null or d.dateArrival <= :dateArrivalTo) "
			+ " AND (coalesce(:dateIssuedFrom, null) is null or d.dateIssued >= :dateIssuedFrom) "
			+ " AND (coalesce(:dateIssuedTo, null) is null or d.dateIssued <= :dateIssuedTo) "
			+ " AND (coalesce(:dateReceivedFrom, null) is null or d.receivedDate >= :dateReceivedFrom) "
			+ " AND (coalesce(:dateReceivedTo, null) is null or d.receivedDate <= :dateReceivedTo) "
			+ " and ((:docFieldsId) is null or d.docFieldsId = (:docFieldsId)) and (d.clientId = :clientId ) and d.active = true"
			+ " and (d.id in (select sp.docId from DocumentInProcess sp where sp.clientId = :clientId and (sp.toUser = :userId or sp.frUser = :userId) group by sp.docId )"
			+ " )")
	Page<Documents> findAll(String numberOrSign, String preview, Long docFieldsId, DocumentStatusEnum docStatusId,
			Long urgentId, Long securityId, Long userId, Long placeSendId, Date dateArrivalFrom, Date dateArrivalTo, Date dateIssuedFrom, Date dateIssuedTo,
			Date dateReceivedFrom, Date dateReceivedTo, Long clientId, Pageable castToPageable);

	@Query("SELECT new com.vz.backend.business.dto.DocInRetakeDto(d.id, d.updateDate, d.preview, d.orgIssuedName, d.docFieldsId, d.docTypeId, d.numberArrival, d.documentBook.numberOrSign, d.numberOrSign, d.dateIssued, d.placeSend, d.numberArrivalStr) "
			+ "FROM Documents d LEFT JOIN ClericalOrg c ON :clericalOrg is true AND c.userId = :userId AND c.active is true "
			+ "WHERE d.clientId = :clientId AND d.status in (:status) AND d.active = TRUE "
			+ "AND((:clericalOrg is true AND d.orgReceiveId = c.orgId) OR (:clericalOrg is false AND d.orgReceiveId = :orgId)) ")
	Page<DocInRetakeDto> listRetake(boolean clericalOrg, DocumentStatusEnum[] status, Long userId, Long orgId, Long clientId, Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.DocInRetakeDto(d.id, d.updateDate, d.preview, d.orgIssuedName, d.docFieldsId, d.docTypeId, d.numberArrival, d.documentBook.numberOrSign, d.numberOrSign, d.dateIssued, d.placeSend, d.numberArrivalStr) "
			+ "FROM Documents d LEFT JOIN ClericalOrg c ON :clericalOrg is true AND c.userId = :userId AND c.active is true "
			+ "WHERE d.clientId = :clientId AND d.status in (:status) AND d.active = TRUE "
			+ "AND((:clericalOrg is true AND d.orgReceiveId = c.orgId) OR (:clericalOrg is false AND d.orgReceiveId = :orgId)) "
			+ "AND (:text is null or lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text%)")
	Page<DocInRetakeDto> quickRetake(boolean clericalOrg, String text, DocumentStatusEnum[] status, Long userId, Long orgId, Long clientId,
			Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.DocInRetakeDto(d.id, d.updateDate, d.preview, d.orgIssuedName, d.docFieldsId, d.docTypeId, d.numberArrival, d.documentBook.numberOrSign, d.numberOrSign, d.dateIssued, d.placeSend, d.numberArrivalStr) "
			+ "FROM Documents d LEFT JOIN ClericalOrg c ON :clericalOrg is true AND c.userId = :userId AND c.active is true "
			+ "WHERE d.clientId = :clientId AND d.status in (:status) "
			+ "AND((:clericalOrg is true AND d.orgReceiveId = c.orgId) OR (:clericalOrg is false AND d.orgReceiveId = :orgId)) "
			+ "AND (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign%) AND d.active = TRUE "
			+ " and (:preview is null or lower(d.preview) like %:preview%) "
			+ " and (:bookId is null or d.bookId = :bookId) and (:docTypeId is null or d.docTypeId = :docTypeId) "
			+ " and (:orgIssuedName is null or lower(d.placeSend) like %:orgIssuedName%) "
			+ " and (:docFieldsId is null or d.docFieldsId = :docFieldsId) "
			+ " and (:docStatusId is null or d.status = :docStatusId) "
			+ " and (:urgentId is null or d.urgentId = :urgentId) "
			+ " and (:securityId is null or d.securityId = :securityId) "
			+ " and (:numberArrival is null or concat(d.numberArrival, lower(d.documentBook.numberOrSign)) like  %:numberArrival%)"
			+ " and (:personSign is null or lower(d.personSign) like %:personSign%) "
			+ " and (coalesce(:startArrival, null) is null or d.dateArrival > :startArrival) "
			+ " and (coalesce(:endArrival, null) is null or d.dateArrival < :endArrival) "
			+ " and (coalesce(:startIssued, null) is null or d.dateIssued > :startIssued) "
			+ " and (coalesce(:endIssued, null) is null or d.dateIssued < :endIssued) ")
	Page<DocInRetakeDto> searchRetake(boolean clericalOrg, String numberOrSign, String preview, Long bookId, Long docTypeId,
			String orgIssuedName, Long docFieldsId, DocumentStatusEnum docStatusId, Long urgentId, Long securityId,
			String numberArrival, Date startArrival, Date endArrival, Date startIssued, Date endIssued,
			String personSign, DocumentStatusEnum[] status, Long userId, Long orgId, Long clientId, Pageable pageable);

	@Query("select new com.vz.backend.business.dto.CategoryDto(p.docId, p.toUsers.fullName)"
			+ " from DocumentInProcess p "
			+ " where p.active = true and p.clientId =:clientId and p.docId in (:docIdList) and p.handleType IN ('MAIN') and p.handleStatus != 'DA_TRA_LAI'"
			+ " and (p.docId, p.step) in (select sp.docId, max(sp.step) from DocumentInProcess sp where sp.clientId = :clientId and sp.active = true and sp.handleType IN ('MAIN') and sp.handleStatus != 'DA_TRA_LAI' and sp.docId in (:docIdList) group by sp.docId)"
			+ " group by p.docId, p.toUsers.fullName")
	List<CategoryDto> findNextHandle(List<Long> docIdList, Long clientId);

	@Query("select d from Documents d where d.clientId = :clientId and d.id in (:input) and d.active = true")
	List<Documents> findByDocIdList(List<Long> input, Long clientId);

	@Query("select new com.vz.backend.business.dto.DocumentDto(d, p.id, du.important, p.updateDate, p.toUsers.fullName, p.progress, p.comment, p.handleStatus, p.deadline)"
			+ " from Documents d join DocumentInProcess p on p.docId = d.id AND p.active = TRUE "
			+ "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = :docType "
			+ " where (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign%) "
			+ " and (:preview is null or lower(d.preview) like %:preview%) "
			+ " AND (:dayLeft IS NULL"
			+ "		OR :dayLeft = "
			+ "				CASE WHEN (COALESCE(p.deadline, NULL) IS NOT NULL AND (p.deadline < current_date())) THEN -1 "
			+ "					 WHEN (COALESCE(p.deadline, NULL) IS NULL OR p.deadline > current_date() + 2)  THEN 3 "
			+ "					 ELSE 0 END)"
			+ " and (:docTypeId is null or d.docTypeId = :docTypeId) "
			+ " and (:docFieldsId is null or d.docFieldsId = :docFieldsId) "
			+ " and (d.clientId = :clientId and p.clientId = :clientId ) "
			+ " and ((:docStatusIds) is null or d.status in (:docStatusIds)) "
			+ " and ((:toUsers) is null or p.delegaterId in (:toUsers) or p.toUser in (:toUsers) ) and p.handleStatus in (:handleStatus)"
			+ " and ((:handleType) is null or p.handleType in (:handleType)) and d.active = true "
			+ "AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
			+ " and (d.id, p.step) in (select sp.docId, max(sp.step) from DocumentInProcess sp where sp.clientId = :clientId and sp.active = true and ((:toUsers) is null or sp.delegaterId in (:toUsers) or sp.toUser in (:toUsers)) group by sp.docId)")
	Page<DocumentDto> findProcessAdvance1(Integer dayLeft, Boolean important, DocumentTypeEnum docType, Long userId,
			List<DocumentStatusEnum> docStatusIds,
			List<Long> toUsers,
			List<DocumentInHandleStatusEnum> handleStatus,
			List<HandleTypeEnum> handleType,
			String numberOrSign, String preview,
			Long docTypeId, Long docFieldsId,
			Long clientId, Pageable pageable);

	@Query("select new com.vz.backend.business.dto.DocumentDto(d, p.id, du.important, p.updateDate, p.toUsers.fullName, p.progress, p.comment, p.handleStatus, p.deadline)"
			+ " from Documents d join DocumentInProcess p on p.docId = d.id AND p.active = TRUE "
			+ "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = :docType "
			+ " where (:text is null or lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text%) "
			+ " and (d.clientId = :clientId and p.clientId = :clientId ) "
			+ " AND (:dayLeft IS NULL"
			+ "		OR :dayLeft = "
			+ "				CASE WHEN (COALESCE(p.deadline, NULL) IS NOT NULL AND (p.deadline < current_date())) THEN -1 "
			+ "					 WHEN (COALESCE(p.deadline, NULL) IS NULL OR p.deadline > current_date() + 2)  THEN 3 "
			+ "					 ELSE 0 END)"
			+ " and ((:docStatusIds) is null or d.status in (:docStatusIds)) "
			+ " and ((:toUsers) is null or p.delegaterId in (:toUsers) or p.toUser in (:toUsers)) and p.handleStatus in (:handleStatus)"
			+ " and ((:handleType) is null or p.handleType in (:handleType)) and d.active = true "
			+ " and (d.id, p.step) in (select sp.docId, max(sp.step) from DocumentInProcess sp where sp.clientId = :clientId and sp.active = true and ((:toUsers) is null or sp.delegaterId in (:toUsers) or sp.toUser in (:toUsers)) group by sp.docId)")
	Page<DocumentDto> findProcessBasic1(Integer dayLeft, DocumentTypeEnum docType, Long userId,
			List<DocumentStatusEnum> docStatusIds,
			List<Long> toUsers,
			List<DocumentInHandleStatusEnum> handleStatus,
			List<HandleTypeEnum> handleType, String text,
			Long clientId, Pageable pageable);

	@Query(value = "select d from Documents d  where d.id  in (:docIds) and d.updateBy in(:userBanThuBan) and d.placeSend is not null order by d.createDate desc")
	Page<Documents> findAllDoc(List<Long> docIds,List<Long> userBanThuBan, Pageable castToPageable);

	@Query(value = "select d from Documents d  where d.id  in (:docIds) and d.updateBy in(:userBanThuBan) and d.placeSend is not null order by d.createDate desc")
	List<Documents> findAllDocNoPage(List<Long> docIds,List<Long> userBanThuBan);

	@Query(value = "select d from Documents d "
			+ " where ((:numberOrSign) is null or lower(d.numberOrSign) like %:numberOrSign%) "
			+ " and ((:preview) is null or lower(d.preview) like %:preview%) "
			+ " and ((:docStatusId) is null or d.status = (:docStatusId)) and d.status is not 'RETAKE_DOC' "
			+ " and ((:urgentId) is null or d.urgentId = (:urgentId)) "
			+ " and ((:securityId) is null or d.securityId = (:securityId)) "
			+ " and (:orgReceiveId is null or d.orgReceiveId = :orgReceiveId or d.id in (select dip.docId from DocumentInProcess dip where dip.toUsers.org = :orgReceiveId)) "
			+ " and ((:docFieldsId) is null or d.docFieldsId = (:docFieldsId)) and (d.clientId = :clientId ) "
			+ " and (:orgIssuedName is null or lower(d.placeSend) like %:orgIssuedName%) "
			+ " and (:bookId is null or d.bookId = :bookId) "
			+ " and (:numberArrival is null or (concat(d.numberArrival,lower(d.documentBook.numberOrSign))) like  %:numberOrSign%)"
//			+ " and (:numberArrivalStr is null or d.numberArrivalStr like %:numberArrivalStr%) "
			+ " and (coalesce(:startArrival, null) is null or d.dateArrival > :startArrival) "
			+ " and (coalesce(:endArrival, null) is null or d.dateArrival < :endArrival) "
			+ " and (coalesce(:startIssued, null) is null or d.dateIssued > :startIssued) "
			+ " and (coalesce(:endIssued, null) is null or d.dateIssued < :endIssued) "
			+ " and (coalesce(:startReceived, null) is null or d.receivedDate > :startReceived) "
			+ " and (:expired is null OR (:expired is true AND d.deadline != null AND (:now > d.deadline)) OR (:expired is false AND (d.deadline is null OR (:now <= d.deadline)))) "
			+ " and (coalesce(:endReceived, null) is null or d.receivedDate < :endReceived) "
			+ " and (:orgIssuedName is null or lower(d.placeSend) like %:orgIssuedName%) "
			+ " and d.active = true "
			+" and ( (d.personEnter.org in (:orgIds)) or  (d.createBy in (select u.id from Category c inner join User u on u.position = c.id where lower(c.name) = :name and u.org = :orgBanId)))")
	List<Documents> findAllIdDoc(Boolean expired , Date now, String numberOrSign, Long bookId,
								 String preview, Long docFieldsId,
								 DocumentStatusEnum docStatusId, Long urgentId,
								 Long securityId, Long clientId, Long numberArrival, Date startArrival,
								 Date endArrival, Date startIssued, Date endIssued, Date startReceived, Date endReceived,
								 String orgIssuedName, List<Long> orgIds, String name, Long orgBanId, Long orgReceiveId);

	@Query(value = "SELECT new com.vz.backend.business.dto.DocumentInReceiveDto(d, du.important, ob.read) FROM Documents d "
			+ "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = :docType "
			+ "LEFT JOIN ObjectRead ob ON ob.objId = d.id AND ob.userId = :userId AND ob.type = 'VAN_BAN_DEN' "
			+ "LEFT JOIN ClericalOrg c ON :clericalOrg is true AND c.userId = : userId AND c.active is true AND d.clientId = :clientId "
			+ "WHERE (lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text%) "
			+ "AND (d.mergedLines IS NULL OR d.mergedLines IS FALSE) "
			+ "AND ((:clericalOrg is true AND d.orgReceiveId = c.orgId) OR (:clericalOrg is false AND d.orgReceiveId = :org)) "
			+ "AND d.status IN (:status) AND d.active = true")
	Page<DocumentInReceiveDto> findWaitToReceive(boolean clericalOrg, DocumentTypeEnum docType, Long userId, String text, Long org,
			List<DocumentStatusEnum> status, Long clientId, Pageable pageable);

	@Query(value = "SELECT new com.vz.backend.business.dto.DocumentInReceiveDto(d, du.important, ob.read) FROM Documents d "
			+ "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = :docType "
			+ "LEFT JOIN ObjectRead ob ON ob.objId = d.id AND ob.userId = :userId AND ob.type = 'VAN_BAN_DEN' "
			+ "LEFT JOIN ClericalOrg c ON :clericalOrg is true AND c.userId = : userId AND c.active is true "
			+ "WHERE (d.mergedLines IS NULL OR d.mergedLines IS FALSE) AND d.clientId = :clientId "
			+ "AND (:numberOrSign is null OR lower(d.numberOrSign) like %:numberOrSign%) "
			+ "AND (:preview is null OR lower(d.preview) like %:preview%) "
			+ "AND (:docTypeId is null OR d.docTypeId = :docTypeId) "
			+ "AND (:docFieldsId is null OR d.docFieldsId = :docFieldsId) "
			+ "AND (:methodReceiptId is null OR d.methodReceiptId = :methodReceiptId) "
			+ "AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
			+ "AND (:expired is null OR (:expired is true AND d.deadline != null AND (:now > d.deadline)) OR (:expired is false AND (d.deadline is null OR (:now <= d.deadline)))) "
			+ "AND ((:clericalOrg is true AND d.orgReceiveId = c.orgId) OR (:clericalOrg is false AND d.orgReceiveId = :org)) "
			+ "AND d.status IN (:status) AND d.active = true")
	Page<DocumentInReceiveDto> findWaitToReceive(boolean clericalOrg, Boolean important, Boolean expired, Date now, DocumentTypeEnum docType,
			Long userId, String numberOrSign, String preview, Long docTypeId, Long docFieldsId, Long methodReceiptId,
			Long org, List<DocumentStatusEnum> status, Long clientId, Pageable pageable);

	boolean existsByIdAndStatusIn(Long id, List<DocumentStatusEnum> listStatus);

	@Query("select d from Documents d "
			+ " LEFT JOIN ClericalOrg c ON :clericalOrg is true AND c.userId = :userId AND c.active is true "
			+ " where (:clientId is null or d.clientId = :clientId ) and d.active = true "
			+ " and d.status != :retake"
			+ " and ( d.id in (select sp3.docId from DocumentInProcess sp3 where sp3.active = TRUE AND sp3.clientId = :clientId and (sp3.toUser in (:userIds)) group by sp3.docId )"
			+ " or d.id in (select sd.id from Documents sd where sd.clientId = :clientId and sd.active = true "
			+ " and ((:clericalOrg is true AND sd.orgReceiveId = c.orgId) OR (:clericalOrg is false AND sd.personEnterId in (:userIds))) "
			+ " and :isLibrarian = true and sd.status = :notyet) )")
	List<Documents> findByUserIdAndClientId(boolean clericalOrg, Long userId, DocumentStatusEnum retake,
			DocumentStatusEnum notyet, Boolean isLibrarian, List<Long> userIds, Long clientId);

	List<Documents> findByClientIdAndActiveTrueAndIdIn(Long clientId, List<Long> ids);
	
	@Query("SELECT NEW com.vz.backend.business.dto.ReportDocByTypeDto('DOC_IN_DELEGATE', COUNT(DISTINCT p.docId))"
			+ " FROM Documents d JOIN DocumentInProcess p on p.docId = d.id AND p.active = TRUE "
			+ " LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :toUsers AND du.docType = 'VAN_BAN_DEN' "
			+ " WHERE (d.clientId = :clientId and p.clientId = :clientId ) "
			+ " AND ((:docStatusIds) is null or d.status in (:docStatusIds)) "
			+ " AND ((:toUsers) is null or p.delegaterId = (:toUsers)) and p.handleStatus in (:handleStatus)"
			+ " AND ((:handleType) is null or p.handleType in (:handleType)) and d.active = true "
			+ " AND (d.id, p.step) in (SELECT sp.docId, max(sp.step) FROM DocumentInProcess sp WHERE sp.clientId = :clientId and sp.active = true and ((:toUsers) is null or sp.delegaterId = (:toUsers)) GROUP BY sp.docId)")
	ReportDocByTypeDto reportDocDelegate(Long toUsers, List<DocumentStatusEnum> docStatusIds,
			List<DocumentInHandleStatusEnum> handleStatus, List<HandleTypeEnum> handleType, Long clientId);
	
	@Query("SELECT NEW com.vz.backend.business.dto.document.ResolvedUserDto(p.toUsers, COUNT(a)>0) FROM DocumentInProcess p LEFT JOIN AuthorityUser a ON p.toUser = a.userId"
			+ " WHERE p.clientId=:clientId AND p.active =TRUE AND p.docId=:docId AND (a IS NULL OR a.authority ='LEADERSHIP' ) AND ( a IS NOT NULL OR p.handleType = 'SHOW' OR p.handleType = 'SUPPORT') "
			+ " AND (a IS NULL OR a.active=TRUE )"
			+ " GROUP BY p.toUsers"
			)
	List<ResolvedUserDto> getUserInfoByDocId(Long docId, Long clientId);
	
	@Query("SELECT d FROM Documents d WHERE d.clientId = :clientId AND d.numberArrival = :numberInBook AND d.bookId = :bookId AND d.orgReceiveId = :orgId AND d.active = TRUE")
	Documents getDocInByDocOutAndOrgId(Long bookId, Long numberInBook, Long orgId, Long clientId);

	@Query("SELECT d FROM Documents d WHERE d.clientId = :clientId and d.active = TRUE and d.parentId=:parentId")
	List<Documents> findByParentIdAndActive(Long parentId, Long clientId);

	Documents findFirstByClientIdAndParentIdAndOrgReceiveIdAndActiveTrue(Long clientId, Long parentId, Long orgReceiveId);

	@Query("SELECT NEW com.vz.backend.core.dto.IdName(p.docId, p.toUsers.orgModel.name) FROM DocumentInProcess p WHERE p.docId IN (:docIds) AND p.clientId = :clientId AND p.active = TRUE "
			+ " AND (p.docId, p.step) IN (SELECT sp.docId, max(sp.step) FROM DocumentInProcess sp WHERE sp.clientId = :clientId AND sp.active = true AND p.docId IN (:docIds) AND p.handleType IN ('MAIN', 'SUPPORT', 'SHOW') GROUP BY sp.docId)"
			)
	List<IdName> getOrgMappingDocId(List<Long> docIds, Long clientId);
	
	@Query("SELECT sp4.docId FROM DocumentInProcess sp4 "
			+ " WHERE sp4.active = TRUE AND sp4.clientId = :clientId "
			+ " AND (:handleType IS NULL OR handleType = :handleType) "
			+ " AND (:orgExe IS NULL OR LOWER(sp4.toUsers.orgModel.name) like %:orgExe%) "
			+ " AND (:userExe IS NULL OR LOWER(sp4.toUsers.fullName) like %:userExe%) "
			+ " AND (:handleStatus IS NULL OR sp4.handleStatus = :handleStatus) "
			+ " AND (:endTask IS NULL"
			+ "		OR :endTask = "
			+ "				CASE WHEN sp4.document.status IN ('DOING', 'RETURN_DOC') AND sp4.endTask IS TRUE THEN 2 "
			+ "					 WHEN sp4.document.status = 'DONE' THEN 2 "
			+ "					 WHEN sp4.document.status IN ('DOING', 'RETURN_DOC') AND sp4.endTask IS NULL THEN 0 "
			+ "					 ELSE NULL END ) "
			+ " AND (:delegateDoc IS NULL OR sp4.delegaterId = :userId) GROUP BY sp4.docId")
	List<Long> getWithProcess(Boolean delegateDoc, String orgExe, String userExe, HandleTypeEnum handleType,
			DocumentInHandleStatusEnum handleStatus, Integer endTask, Long userId, Long clientId);

	@Query("SELECT sp4.docId FROM DocumentInProcess sp4 "
			+ " WHERE sp4.active = TRUE AND sp4.clientId = :clientId "
			+ " AND (:handleType IS NULL OR handleType = :handleType) "
//			+ " AND (:orgExe IS NULL OR LOWER(sp4.toUsers.orgModel.name) like %:orgExe%) "
//			+ " AND (:userExe IS NULL OR LOWER(sp4.toUsers.fullName) like %:userExe%) "
			+ " AND (:handleStatus IS NULL OR sp4.handleStatus = :handleStatus) "
			+ " AND (:endTask IS NULL"
			+ "		OR :endTask = "
			+ "				CASE WHEN sp4.document.status IN ('DOING', 'RETURN_DOC') AND sp4.endTask IS TRUE THEN 2 "
			+ "					 WHEN sp4.document.status = 'DONE' THEN 2 "
			+ "					 WHEN sp4.document.status IN ('DOING', 'RETURN_DOC') AND sp4.endTask IS NULL THEN 0 "
			+ "					 ELSE NULL END ) "
			+ " AND (:delegateDoc IS NULL OR sp4.delegaterId = :userId) GROUP BY sp4.docId")
	List<Long> getWithProcesss(Boolean delegateDoc, HandleTypeEnum handleType,
							  DocumentInHandleStatusEnum handleStatus, Integer endTask, Long userId, Long clientId);
	@Query("SELECT max(doc.numberArrival) FROM Documents doc "
			+ " WHERE doc.active = TRUE AND doc.clientId = :clientId and doc.bookId = :bookId")
	Long getMaxNumberArrivalOfDocumentByBookId(Long clientId, Long bookId);


	@Query("select d from Documents d where d.docOutId = :id and d.clientId = :clientId")
    List<Documents> findByDocOutId(Long id, Long clientId);

	@Query("select count(d) > 0 from Documents d where d.numberOrSign = :numberOrSign and d.clientId = :clientId and d.active = :active and (:excludeDocIds is null or d.id not in (:excludeDocIds))")
	boolean isNumberOrSignExists(String numberOrSign, Long clientId, List<Long> excludeDocIds, boolean active);
}
