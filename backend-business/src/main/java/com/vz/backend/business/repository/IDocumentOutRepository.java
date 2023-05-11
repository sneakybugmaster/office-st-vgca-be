package com.vz.backend.business.repository;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.Documents;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.dto.DocOutDto;
import com.vz.backend.business.dto.DocumentOutDto;
import com.vz.backend.business.dto.KnowableDto;
import com.vz.backend.business.dto.UserConditionDto;
import com.vz.backend.core.config.AuthorityEnum;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.dto.SignerDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentOutRepository extends IRepository<DocumentOut> {

    @Query("Select d from DocumentOut d where d.id = :docId and (:clientId is null or d.clientId = :clientId)")
    DocumentOut findByDocIdAndClientId(Long docId, Long clientId);

    @Query("select d from DocumentOut d "
            + "LEFT JOIN DocumentUser du ON du.docId = d.id  AND du.userId = :user AND du.docType = :docType "
            + " LEFT JOIN DocumentOutProcess p ON p.docId = d.id "
            + " where (:text is null or lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text%)"
            + " and (:clientId is null or d.clientId = :clientId ) " + " and (d.status in (:status)) "
            + " and (coalesce(:user, null) is null or d.personEnterId = (:user)) " + " and d.active = true"
            + " and (:orgId is null OR d.orgIssuedId = :orgId) "
            + "AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
            + " AND (p IS NULL OR p.handleStatus IN (:handleStatus))"
            + " AND (p IS NULL OR p.active = TRUE)")
    Page<DocumentOut> getListDocBasic1(Boolean important, DocumentTypeEnum docType, Long user,
            String text, Long clientId,
            DocumentStatusEnum[] status, Long orgId,
            DocumentOutHandleStatusEnum[] handleStatus,
            Pageable castToPageable);

    @Query("select d from DocumentOut d LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :user AND du.docType = :docType "
            + "LEFT JOIN ClericalOrg c ON :clericalOrg is true AND c.userId = :user AND c.active is true " +
            " left join Documents din on din.docOutId = d.id and din.active is true "
            + " where (:text is null or lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text%) "
            + " and (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign%)"
            + " and (:preview is null or lower(d.preview) like %:preview%) "
            + " and (:orgCreateName is null or lower(d.orgCreateName)  = :orgCreateName) "
            + " and (:clientId is null or d.clientId = :clientId ) "
            + " and (:docTypeId is null or d.docTypeId = :docTypeId) "
            + " and (:docFieldId is null or d.docFieldId = :docFieldId) "
            + " and (:bookId is null or d.bookId = :bookId) "
            + " and (coalesce(:startCreate, null) is null or d.createDate > :startCreate) "
            + " and (coalesce(:endCreate, null) is null or d.createDate < :endCreate) "
            + " and (coalesce(:startIssued, null) is null or d.dateIssued > :startIssued) "
            + " and (coalesce(:endIssued, null) is null or d.dateIssued < :endIssued) "
            + " and (coalesce(:docOutIds, null) is null or d.id in (:docOutIds)) and (d.status in (:status)) and d.active = true "
            + "AND ((:clericalOrg is true AND d.orgIssuedId = c.orgId) OR (:clericalOrg is false AND d.orgIssuedId = :orgId)) "
            + "AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
            +
            " and ((:isInternalDocument is true and d.isInternalDocument is true and ((:getFinishedInternalDoc is true and din.status = 'DONE') or (:getFinishedInternalDoc is false and din.status <> 'DONE' or din = null))) or (:isInternalDocument is false and (d.isInternalDocument is false or d.isInternalDocument is null))) group by d.id")
    Page<DocumentOut> getListDocAdvance(String text, boolean clericalOrg, DocumentTypeEnum docType, Boolean important,
            List<Long> docOutIds, Long user, String numberOrSign, String preview, String orgCreateName,
            Date startCreate, Date endCreate, Date startIssued, Date endIssued, Long docTypeId, Long docFieldId,
            Long bookId, DocumentStatusEnum[] status, Boolean getFinishedInternalDoc, Long clientId, Long orgId,
            Boolean isInternalDocument, Pageable castToPageable);

    @Query("select d from DocumentOut d "
            + " LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :user AND du.docType = :docType "
            + " LEFT JOIN DocumentOutProcess p ON p.docId = d.id "
            + " where (:text is null or lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text%)"
            + " AND (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign%)"
            + " and (:preview is null or lower(d.preview) like %:preview%) "
            + " and (:orgCreateName is null or lower(d.orgCreateName)  = :orgCreateName) "
            + " and (:clientId is null or d.clientId = :clientId ) "
            + " and (:docTypeId is null or d.docTypeId = :docTypeId) "
            + " and (:docFieldId is null or d.docFieldId = :docFieldId) "
            + " and (:bookId is null or d.bookId = :bookId) "
            + " and (coalesce(:startCreate, null) is null or d.createDate > :startCreate) "
            + " and (coalesce(:endCreate, null) is null or d.createDate < :endCreate) "
            + " and (coalesce(:startIssued, null) is null or d.dateIssued > :startIssued) "
            + " and (coalesce(:endIssued, null) is null or d.dateIssued < :endIssued) "
            + " and (coalesce(:docOutIds, null) is null or d.id in (:docOutIds)) "
            + " and (coalesce(:user, null) is null or d.personEnterId = (:user)) " + " and (d.status in (:status)) "
            + " and d.active = true" + " and (:orgId is null OR d.orgIssuedId = :orgId) "
            + " AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
            + " AND (p IS NULL OR p.handleStatus IN (:handleStatus))"
            + " AND (p IS NULL OR p.active = TRUE)")
    Page<DocumentOut> getListDraft(String text, Boolean important, DocumentTypeEnum docType, List<Long> docOutIds,
            Long user, String numberOrSign, String preview, String orgCreateName, Date startCreate, Date endCreate,
            Date startIssued, Date endIssued, Long docTypeId, Long docFieldId, Long bookId, DocumentStatusEnum[] status,
            Long clientId, Long orgId, DocumentOutHandleStatusEnum[] handleStatus, Pageable castToPageable);

    @Query("select d  from DocumentOut d "
            + " LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = :docType "
            + " LEFT JOIN DocumentOutProcess p ON p.docId = d.id "
            + " where (:text is null or lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text%) "
            + " and (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign%)"
            + " and (:preview is null or lower(d.preview) like %:preview%) "
            + " and (:orgCreateName is null or lower(d.orgCreateName)  = :orgCreateName) "
            + " and (:clientId is null or d.clientId = :clientId ) "
            + " and (:docTypeId is null or d.docTypeId = :docTypeId) "
            + " and (:docFieldId is null or d.docFieldId = :docFieldId) "
            + " and (:bookId is null or d.bookId = :bookId) "
            + " and (coalesce(:startCreate, null) is null or d.createDate > :startCreate) "
            + " and (coalesce(:endCreate, null) is null or d.createDate < :endCreate) "
            + " and (coalesce(:startIssued, null) is null or d.dateIssued > :startIssued) "
            + " and (coalesce(:endIssued, null) is null or d.dateIssued < :endIssued) "
            + " and (coalesce(:docOutIds, null) is null or d.id in (:docOutIds)) "
            + " and (d.personEnterId = :userId OR p.userId = :userId ) "
            + " and (d.status in (:status)) "
            + " and d.active = true" + " and (:orgId is null OR d.orgIssuedId = :orgId) "
            + " AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
            + " AND (p IS NULL OR (:handleStatus) IS NULL OR p.handleStatus IN (:handleStatus))"
            + " AND (p IS NULL OR p.active = TRUE) "
            + " GROUP BY d, du.important, d.userEnter.fullName, d.docType.name, d.security.name ")
    Page<DocumentOut> getIssuedDocsAdvance(String text, Boolean important, DocumentTypeEnum docType,
            List<Long> docOutIds, Long userId, String numberOrSign, String preview, String orgCreateName,
            Date startCreate, Date endCreate, Date startIssued, Date endIssued, Long docTypeId, Long docFieldId,
            Long bookId, DocumentStatusEnum[] status, Long clientId, Long orgId,
            List<DocumentOutHandleStatusEnum> handleStatus, Pageable castToPageable);

    @Query("SELECT new com.vz.backend.business.dto.DocOutDto(0, d.id, d.numberInBook, d.numberOrSign, d.preview, c.name, d.userEnter.userName, d.createDate, d.dateIssued, d.urgentId, d.securityId) "
            + "FROM DocumentOut d JOIN Category c ON d.docTypeId = c.id "
            + "WHERE d.userEnter.userName = :personEnter AND d.clientId = :clientId AND d.status = :status ORDER BY d.id DESC")
    Page<DocOutDto> getListDocOut(String personEnter, Long clientId, DocumentStatusEnum status,
            Pageable castToPageable);

    @Query("SELECT new com.vz.backend.business.dto.DocOutDto(0, d.id, d.numberInBook, d.numberOrSign, d.preview, c.name, d.userEnter.userName, d.createDate, d.dateIssued, d.urgentId, d.securityId) "
            + "FROM DocumentOut d JOIN Category c ON d.docTypeId = c.id "
            + "LEFT JOIN ClericalOrg co ON :clericalOrg is true AND co.userId = :userId AND co.active is true "
            + "WHERE (:text is null OR lower(d.numberOrSign) like %:text% OR lower(d.preview) like %:text%) "
            + "AND ((:clericalOrg is true AND d.orgIssuedId = co.orgId) OR (:clericalOrg is false AND d.orgIssuedId = :orgId)) "
            + "AND d.clientId = :clientId AND d.status = :status AND d.active = TRUE")
    Page<DocOutDto> getListIssued(boolean clericalOrg, String text, Long userId, Long orgId, Long clientId,
            DocumentStatusEnum status,
            Pageable castToPageable);

    @Query("SELECT new com.vz.backend.business.dto.DocOutDto(0, d.id, d.numberInBook, d.numberOrSign, d.preview, c.name, d.userEnter.userName, d.createDate, d.dateIssued, d.urgentId, d.securityId) "
            + "FROM DocumentOut d JOIN Category c ON d.docTypeId = c.id "
            + "LEFT JOIN ClericalOrg co ON :clericalOrg is true AND co.userId = :userId AND co.active is true "
            + "WHERE (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign%) "
            + "AND (:orgCreateName is null or lower(d.orgCreateName) = :orgCreateName) "
            + "AND (:personEnter is null or lower(d.userEnter.userName) = :personEnter) "
            + "AND (:preview is null or lower(d.preview) like %:preview%) "
            + "AND (coalesce(:startCreate, null) is null or d.createDate > :startCreate) "
            + "AND (coalesce(:endCreate, null) is null or d.createDate < :endCreate) "
            + "AND (coalesce(:startIssued, null) is null or d.dateIssued > :startIssued) "
            + "AND (coalesce(:endIssued, null) is null or d.dateIssued < :endIssued) "
            + "AND (:docTypeId is null or d.docTypeId = :docTypeId) "
            + "AND (:docFieldId is null or d.docFieldId = :docFieldId) "
            + "AND (:bookId is null or d.bookId = :bookId) "
            + "AND ((:clericalOrg is true AND d.orgIssuedId = co.orgId) OR (:clericalOrg is false AND d.orgIssuedId = :orgId)) "
            + "AND d.clientId = :clientId AND d.status = :status AND d.active = TRUE ")
    Page<DocOutDto> getListIssued(boolean clericalOrg, String numberOrSign, String orgCreateName, String personEnter,
            String preview,
            Date startCreate, Date endCreate, Date startIssued, Date endIssued, Long docTypeId, Long docFieldId,
            Long bookId, Long userId, Long orgId, Long clientId, DocumentStatusEnum status, Pageable castToPageable);

    @Query("select new com.vz.backend.business.dto.DocumentOutDto(d, p.user.fullName) " + " from DocumentOut d "
            + " join DocumentOutProcess p on d.id = p.docId AND p.userId = :user AND p.active is true "
            + "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :user AND du.docType = :docType "
            + " where (:text is null or lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text%)"
            + " and (d.clientId = :clientId ) "
            + "AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
            + " and (p.clientId = :clientId ) " + " and (coalesce(:user, null) is null or d.personEnterId = (:user)) "
            + " and (coalesce(:user, null) is null or p.userId = (:user)) " + " and d.active = true and p.active = true"
            + " and (d.status in (:docStatus))" + " and (d.id, coalesce(p.updateDate, p.createDate)) in "
            + "(select sp.docId, max(coalesce(sp.updateDate, sp.createDate)) from DocumentOutProcess sp "
            + "   where sp.clientId = :clientId and sp.active = true and sp.userId = (:user) group by sp.docId )")
    Page<DocumentOutDto> getListDocProcessBasicSigner(DocumentTypeEnum docType, Boolean important, Long user,
            String text, Long clientId,
            DocumentStatusEnum[] docStatus, Pageable castToPageable);

    @Query("select new com.vz.backend.business.dto.DocumentOutDto(d, p.user.fullName) " + " from DocumentOut d "
            + " join DocumentOutProcess p on d.id = p.docId AND p.userId = :user AND p.active is true "
            + "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :user AND du.docType = :docType "
            + " where (:text is null or lower(d.numberOrSign) like %:text% or lower(d.preview) like %:text%) "
            + " and (:numberOrSign is null or lower(d.numberOrSign) like %:numberOrSign%)"
            + " and (:preview is null or lower(d.preview) like %:preview%) "
            + " and (:orgCreateName is null or lower(d.orgCreateName) = :orgCreateName) "
            + " and (d.clientId = :clientId ) " + " and (p.clientId = :clientId ) "
            + " and (:docTypeId is null or d.docTypeId = :docTypeId) "
            + " and (:docFieldId is null or d.docFieldId = :docFieldId) "
            + " and (:bookId is null or d.bookId = :bookId) "
            + " and (coalesce(:startCreate, null) is null or d.createDate > :startCreate) "
            + " and (coalesce(:endCreate, null) is null or d.createDate < :endCreate) "
            + " and (coalesce(:startIssued, null) is null or d.dateIssued > :startIssued) "
            + " and (coalesce(:endIssued, null) is null or d.dateIssued < :endIssued) "
            + " and (coalesce(:user, null) != null and p.userId = (:user)) "
            + " and (coalesce(:user, null) is null or d.personEnterId = (:user)) "
            + " and d.active = true and p.active = true "
            + "AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
            + " and (d.status in (:docStatus))" + " and (d.id, coalesce(p.updateDate, p.createDate)) in "
            + "(select sp.docId, max(coalesce(sp.updateDate, sp.createDate)) from DocumentOutProcess sp "
            + "   where sp.clientId = :clientId and sp.active = true and sp.userId = (:user) group by sp.docId ) " +
            " and ((:isInternalDocument is true and d.isInternalDocument is true ) or (:isInternalDocument is false and (d.isInternalDocument is false or d.isInternalDocument is null))) group by d.id, p.user.fullName")
    Page<DocumentOutDto> getListDocProcessAdvanceAssign(String text, DocumentTypeEnum docType, Boolean important,
            Long user, String numberOrSign, String preview, String orgCreateName, Date startCreate, Date endCreate,
            Date startIssued, Date endIssued, Long docTypeId, Long docFieldId, Long bookId,
            DocumentStatusEnum[] docStatus, Long clientId, Boolean isInternalDocument, Pageable castToPageable);

    @Query("SELECT new com.vz.backend.business.dto.KnowableDto(d.id, d.numberInBook, d.numberOrSign, d.docType.id, d.docType.name, d.userEnter.fullName, d.dateIssued, d.preview, d.status, du.important, ob.read, dr.handleType) "
            + "FROM DocumentOut d LEFT JOIN ObjectRead ob ON ob.objId = d.id AND ob.userId = :userId AND ob.type = :docType "
            + "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = :docType "
            + "LEFT JOIN DocumentReceive dr ON dr.docId = d.id AND dr.receiveId = :userId "
            + "WHERE ((:done IS TRUE AND dr.status = 'DONE') OR (:done IS FALSE AND (dr.status IS NULL OR dr.status = 'NOT_YET'))) "
            + "AND (d.id in ("
            + "SELECT d.id FROM DocumentOut d INNER JOIN DocumentReceive r ON d.id=r.docId and d.active=true and r.active=true "
            + "LEFT JOIN User u ON (r.type ='ORG' OR r.type = 'ALL') AND r.receiveId=u.org AND u.id=:userId "
            + "LEFT JOIN Category ca ON u.position=ca.id "
            + "WHERE d.status=:status AND "
            + "((r.type ='FORWARD' AND r.receiveId=:userId) OR (r.type ='USER' AND r.receiveId=:userId) OR (r.type ='ORG' AND ca.isLeadership is TRUE AND ca.isSiblings is TRUE AND r.receiveId=u.org OR (r.type ='ALL' AND r.receiveId=u.org )))"
            + ")"
            + " OR d.id IN (:docIds))")
    Page<KnowableDto> findKnowable(DocumentTypeEnum docType, Long userId, DocumentStatusEnum status, List<Long> docIds,
            boolean done, Pageable pageable);

    @Query("SELECT new com.vz.backend.business.dto.KnowableDto(d.id, d.numberInBook, d.numberOrSign, d.docType.id, d.docType.name, d.userEnter.fullName, d.dateIssued, d.preview, d.status, du.important, ob.read, dr.handleType) "
            + "FROM DocumentOut d LEFT JOIN ObjectRead ob ON ob.objId = d.id AND ob.userId = :userId AND ob.type = :docType "
            + "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = :docType "
            + "LEFT JOIN DocumentReceive dr ON dr.docId = d.id AND dr.receiveId = :userId "
            + "WHERE ((:done IS TRUE AND dr.status = 'DONE') OR (:done IS FALSE AND (dr.status IS NULL OR dr.status = 'NOT_YET'))) "
            + "AND (:text IS NULL OR lower(d.preview) LIKE %:text% OR lower(d.numberOrSign) LIKE %:text%)"
            + "AND (d.id in ("
            + "SELECT d.id FROM DocumentOut d INNER JOIN DocumentReceive r ON d.id=r.docId and d.active=true and r.active=true "
            + "LEFT JOIN User u ON (r.type ='ORG' OR r.type = 'ALL') AND r.receiveId=u.org AND u.id=:userId "
            + "LEFT JOIN Category ca ON u.position=ca.id "
            + "WHERE d.status=:status AND "
            + "((r.type ='FORWARD' AND r.receiveId=:userId) OR (r.type ='USER' AND r.receiveId=:userId) OR (r.type ='ORG' AND ca.isLeadership is TRUE AND ca.isSiblings is TRUE AND r.receiveId=u.org OR (r.type ='ALL' AND r.receiveId=u.org )))"
            + ")"
            + " OR d.id IN (:docIds)) ")
    Page<KnowableDto> quickKnowable(DocumentTypeEnum docType, String text, Long userId, DocumentStatusEnum status,
            List<Long> docIds, boolean done, Pageable pageable);

    @Query("SELECT new com.vz.backend.business.dto.KnowableDto(d.id, d.numberInBook, d.numberOrSign, d.docType.id, d.docType.name, d.userEnter.fullName, d.dateIssued, d.preview, d.status, du.important, ob.read, dr.handleType) "
            + "FROM DocumentOut d LEFT JOIN ObjectRead ob ON ob.objId = d.id AND ob.userId = :userId AND ob.type = :docType "
            + "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = :docType "
            + "LEFT JOIN DocumentReceive dr ON dr.docId = d.id AND dr.receiveId = :userId "
            + "WHERE ((:done IS TRUE AND dr.status = 'DONE') OR (:done IS FALSE AND (dr.status IS NULL OR dr.status = 'NOT_YET'))) "
            + "AND ((:important is null OR du.important = :important) AND " + "d.id in ("
            + "SELECT d.id FROM DocumentOut d INNER JOIN DocumentReceive r ON d.id=r.docId and d.active=true and r.active=true "
            + "LEFT JOIN User u ON (r.type ='ORG' OR r.type = 'ALL') AND r.receiveId=u.org AND u.id=:userId "
            + "LEFT JOIN Category ca ON u.position=ca.id "
            + "INNER JOIN User u2 ON d.personEnterId=u2.id "
            + "WHERE d.status=:status AND d.clientId=:clientId AND (:handleType IS NULL OR r.handleType = :handleType) AND "
            + "((r.type ='FORWARD' AND r.receiveId=:userId) OR (r.type ='USER' AND r.receiveId=:userId) OR (r.type ='ORG' AND ca.isLeadership is TRUE AND ca.isSiblings is TRUE AND r.receiveId=u.org ) OR (r.type ='ALL' AND r.receiveId=u.org ))"
            + "AND (:preview IS NULL OR lower(d.preview) LIKE %:preview%)"
            + "AND (:numberOrSign IS NULL OR lower(d.numberOrSign) LIKE %:numberOrSign%)"
            + "AND (:orgName IS NULL OR lower(d.orgCreateName) LIKE %:orgName%)"
            + "AND (:docTypeId IS NULL OR d.docTypeId = :docTypeId)"
            + "AND (:docFieldId IS NULL OR d.docFieldId = :docFieldId)"
            + "AND (:personEnter IS NULL OR LOWER(u2.userName) = :personEnter OR LOWER(u2.fullName) LIKE '%'||:personEnter||'%')"
            + "AND (coalesce(:startDate, null) IS NULL OR d.createDate > :startDate)"
            + "AND (coalesce(:endDate, null) IS NULL OR d.createDate < :endDate)"
            + "AND (coalesce(:startIssued, null) IS NULL OR d.dateIssued > :startIssued)"
            + "AND (coalesce(:endIssued, null) IS NULL OR d.dateIssued < :endIssued))"
            + " OR d.id IN (:docIds)) ")
    Page<KnowableDto> searchKnowable(DocumentTypeEnum docType, Boolean important, Long clientId,
            String preview, String numberOrSign, Long docTypeId, Long docFieldId, String orgName, String personEnter,
            Date startDate, Date endDate, Date startIssued, Date endIssued, Long userId, DocumentStatusEnum status,
            List<Long> docIds, HandleTypeEnum handleType, boolean done, Pageable pageable);

    public static final String FIND_ALL_QUERY = "SELECT d FROM DocumentOut d INNER JOIN DocumentReceive dr ON d.id = dr.docId "
            + " LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId = :userId AND du.docType = 'VAN_BAN_DI' "
            + " LEFT JOIN ClericalOrg c ON :clericalOrg IS TRUE AND c.userId = :userId and du.userId = c.userId AND c.active IS TRUE "
            + " LEFT JOIN OutsideReceiveDocument o ON o.docId = d.id AND o.active IS TRUE "
            + " LEFT JOIN ObjectTag ot ON ot.objId = d.id AND ot.active = TRUE AND ot.type = 'VAN_BAN_DI' "
            + " LEFT JOIN Tag t ON t.id = ot.tagId AND t.active = TRUE AND t.createBy = :userId "
            + " WHERE (:text IS NULL OR LOWER(d.numberOrSign) LIKE %:text% OR LOWER(d.preview) LIKE %:text% OR lower(t.name) LIKE %:text% ) "
            + " AND (:numberOrSign IS NULL OR LOWER(d.numberOrSign) LIKE %:numberOrSign% OR lower(t.name) LIKE %:numberOrSign% ) "
            + " AND (:important IS NULL OR (:important = FALSE AND du.important IS NULL) OR du.important = :important) "
            + " AND (:preview IS NULL OR LOWER(d.preview) LIKE %:preview% OR lower(t.name) LIKE %:preview% ) "
            + " AND (:outsideReceive IS NULL OR LOWER(o.address) LIKE %:outsideReceive%) "
            + " AND (:orgCreateName IS NULL OR LOWER(d.orgCreateName) LIKE %:orgCreateName%) "
            + " AND (:orgReceiveId is null OR dr.receiveId = :orgReceiveId) "
            + " AND d.clientId = :clientId AND d.active = TRUE "
            + " AND (:docTypeId IS NULL OR d.docTypeId = :docTypeId) "
            + " AND (:docFieldId IS NULL OR d.docFieldId = :docFieldId) "
            + " AND (:status IS NULL OR d.status = :status) "
            + " AND (:bookId IS NULL OR d.bookId = :bookId) "
            + " AND (COALESCE(:startCreate, NULL) IS NULL OR d.createDate > :startCreate) "
            + " AND (COALESCE(:endCreate, NULL) IS NULL OR d.createDate < :endCreate) "
            + " AND (COALESCE(:startIssued, NULL) IS NULL OR d.dateIssued > :startIssued) "
            + " AND (COALESCE(:endIssued, NULL) IS NULL OR d.dateIssued < :endIssued) "
            + " AND ((:personEnter) IS NULL OR  LOWER(d.userEnter.fullName) LIKE %:personEnter%) "
            + " AND ("
            + "  d.id IN (SELECT sd.id FROM DocumentOut sd JOIN DocumentOutProcess p on sd.id = p.docId "
            + "  LEFT JOIN Delegate d1 ON d1.id = p.delegateId AND d1.startDate <= :date AND d1.endDate >= :date "
            + "  LEFT JOIN User u2 ON u2.id = d1.toUserId "
            + "  LEFT JOIN User u1 ON u1.id = p.delegateUserId "
            + "  WHERE (p.userId = :userId OR d1.toUserId =:userId or p.handlerId = :userId) AND p.clientId = :clientId AND sd.clientId=:clientId)"
            + "  OR d.id IN (SELECT dr.docId FROM DocumentReceive dr WHERE dr.receiveId = :userId AND dr.type = 'USER' AND dr.active = TRUE AND dr.clientId = :clientId)"
            + "  OR (d.id in ("
            + "  SELECT d2.id FROM DocumentOut d2 INNER JOIN DocumentReceive r ON d2.id=r.docId and d2.active=true and r.active=true "
            + "  LEFT JOIN User u ON (r.type ='ORG' OR r.type = 'ALL') AND r.receiveId=u.org AND u.id=:userId "
            + "  WHERE "
            + "  ((r.type ='FORWARD' AND r.receiveId=:userId) OR (r.type ='USER' AND r.receiveId=:userId) OR (r.type ='ORG' AND u.lead is TRUE AND r.receiveId=u.org ) OR (r.type ='ALL' AND r.receiveId=u.org )))"
            + "  )"
            // + " OR ((:lead = TRUE) AND d.id IN (SELECT dr2.docId FROM DocumentReceive dr2
            // WHERE dr2.receiveId = :orgId AND dr2.type = 'ORG' AND dr2.active = TRUE AND
            // dr2.clientId=:clientId))"
            + "  OR (d.id IN (SELECT do0.id FROM DocumentOut do0 WHERE do0.personEnterId = (:userId) AND do0.active = TRUE AND do0.clientId =:clientId))"
            + "  OR ((:vanThu = TRUE) AND d.id IN (SELECT do1.id FROM DocumentOut do1 WHERE do1.status IN ('CHO_BAN_HANH', 'DA_BAN_HANH', 'THU_HOI_BH', 'THU_HOI_XL') AND do1.active = TRUE AND do1.clientId = :clientId "
            + "		AND ((:clericalOrg IS TRUE AND do1.orgIssuedId = c.orgId) OR (:clericalOrg is FALSE AND do1.orgIssuedId = :orgId)))) "
            + " OR d.id IN (:docIds))"
            + " GROUP BY d, du.important, d.userEnter.fullName, d.docType.name, d.security.name ";

    @Query(value = FIND_ALL_QUERY)
    Page<DocumentOut> getListAllAdvance(Date date, String text, boolean clericalOrg, String outsideReceive,
            Boolean important, Long orgId, String personEnter, Long userId, boolean vanThu, String numberOrSign,
            String preview, String orgCreateName, Long orgReceiveId, Date startCreate, Date endCreate, Date startIssued,
            Date endIssued,
            Long docTypeId, Long docFieldId, Long bookId, Long clientId, List<Long> docIds, DocumentStatusEnum status,
            Pageable castToPageable);

    @Query("SELECT d.listSignerIds FROM DocumentOut d WHERE d.id = :docId")
    String getListSignerIds(Long docId);

    @Query("SELECT count(*)>0 FROM DocumentOut d WHERE d.id = :docId AND d.status IN (:listStatus)")
    boolean checkStatusInList(Long docId, List<DocumentStatusEnum> listStatus);

    @Query(value = FIND_ALL_QUERY)
    List<DocumentOut> getListAllAdvanceNotPaging(Date date, String text, boolean clericalOrg, String outsideReceive,
            Boolean important, Long orgId, String personEnter, Long userId, boolean vanThu, String numberOrSign,
            String preview, String orgCreateName, Date startCreate, Date endCreate, Date startIssued, Date endIssued,
            Long docTypeId, Long docFieldId, Long bookId, Long clientId, List<Long> docIds, DocumentStatusEnum status,
            Long orgReceiveId);

    @Query(value = "select d from DocumentOut d "
            + " where ((:numberOrSign) is null or lower(d.numberOrSign) like %:numberOrSign%) "
            + " and ((:preview) is null or lower(d.preview) like %:preview%) "
            + " and ((:docStatusId) is null or d.status = (:docStatusId)) "
            + " and ((:urgentId) is null or d.urgentId = (:urgentId)) "
            + " and ((:securityId) is null or d.securityId = (:securityId)) "
            + " AND (:orgIssuedId is null OR d.orgIssuedId = :orgIssuedId) "
            + " AND (coalesce(:createFrom, null) is null or d.createDate >= :createFrom) "
            + " AND (coalesce(:createTo, null) is null or d.createDate <= :createTo) "
            + " AND (coalesce(:issuedDateFrom, null) is null or d.dateIssued >= :issuedDateFrom) "
            + " AND (coalesce(:issuedDateTo, null) is null or d.dateIssued <= :issuedDateTo) "
            + " and ((:docFieldsId) is null or d.docFieldId = (:docFieldsId)) and (d.clientId = :clientId) and d.active = true"
            + " and (d.id in (select sp.docId from DocumentOutProcess sp where sp.clientId = :clientId and (sp.userId = :userId) group by sp.docId )"
            + " )")
    Page<DocumentOut> findAll(String numberOrSign, String preview, Long docFieldsId, DocumentStatusEnum docStatusId,
            Long urgentId, Long securityId, Long userId, Long orgIssuedId, Date createFrom, Date createTo,
            Date issuedDateFrom, Date issuedDateTo, Long clientId, Pageable castToPageable);

    @Query("SELECT d.docType.name FROM DocumentOut d WHERE d.id = :docId")
    String getDocTypeName(Long docId);

    @Query(value = "select d from DocumentOut d  where d.id  in (:docIds) and d.updateBy in(:vanThuBanId) order by d.createDate DESC")
    Page<DocumentOut> findAllDocByIds(List<Long> docIds, List<Long> vanThuBanId, Pageable castToPageable);

    @Query(value = "select d from DocumentOut d INNER JOIN DocumentReceive dr ON d.id = dr.docId "
            + " where ((:numberOrSign) is null or lower(d.numberOrSign) like %:numberOrSign%) "
            + " and ((:preview) is null or lower(d.preview) like %:preview%) "
            + " and ((:docStatusId) is null or d.status = (:docStatusId)) "
            + " and ((:bookId) is null or d.bookId = (:bookId))"
            + " and ((:urgentId) is null or d.urgentId = (:urgentId)) "
            + " and ((:securityId) is null or d.securityId = (:securityId)) "
            + " and ((:docFieldsId) is null or d.docFieldId = (:docFieldsId)) " + " and (d.clientId = :clientId )"
            + " AND (:orgIssuedId is null OR d.orgIssuedId = :orgIssuedId) "
            + " AND (:orgReceiveId is null OR dr.receiveId = :orgReceiveId) "
            + " AND (coalesce(:createFrom, null) is null or d.createDate >= :createFrom) "
            + " AND (coalesce(:createTo, null) is null or d.createDate <= :createTo) "
            // + " AND (:docTypeId is null OR d.docTypeId = :docTypeId) "
            + " and d.active = true "
            + " and d.orgIssuedId in (:orgIds)")
    List<DocumentOut> findAllDoc(String numberOrSign,
            String preview, Long docFieldsId,
            DocumentStatusEnum docStatusId, Long urgentId,
            Long securityId, Long orgIssuedId, Date createFrom, Date createTo, Long bookId, Long clientId,
            List<Long> orgIds, Long orgReceiveId);

    @Query(value = "select d from DocumentOut d "
            + " where ((:numberOrSign) is null or lower(d.numberOrSign) like %:numberOrSign%) "
            + " and ((:preview) is null or lower(d.preview) like %:preview%) "
            + " and ((:docFieldsId) is null or d.docFieldId = (:docFieldsId)) " + " and (d.clientId = :clientId )"
            + " AND (coalesce(:createFrom, null) is null or d.createDate >= :createFrom) "
            + " AND (coalesce(:createTo, null) is null or d.createDate <= :createTo) "
            + " AND (:docTypeId is null OR d.docTypeId = :docTypeId) "
            + " and d.active = true")
    List<DocumentOut> getListsAllAdvanceNotPaging(String numberOrSign,
            String preview, Long docFieldsId,
            Date createFrom, Date createTo, Long docTypeId, Long clientId);

    Boolean existsDocumentOutByIdAndStatusIn(Long docId, List<DocumentStatusEnum> listStatus);

    @Query("SELECT count(*) > 0 FROM DocumentOut d INNER JOIN NodeModel2 n ON n.id = d.nodeId "
            + "WHERE d.id = :docId AND n.importDocBook is TRUE AND d.numberInBook is NULL and d.status != 'DA_BAN_HANH'")
    boolean checkImportDocBookByDocId(Long docId);

    @Query("select d from DocumentOut d "
            + " LEFT JOIN ClericalOrg c ON :clericalOrg is true AND c.userId = :userId AND c.active is true "
            + " where d.clientId = :clientId and d.status not in (:docStatusId) "
            + " and ("
            + "  d.id in (select sd.id from DocumentOut sd join DocumentOutProcess p on sd.id = p.docId where p.user.id = :userId and p.active = true and p.clientId = :clientId and sd.clientId=:clientId)"
            + "  or d.id in (select dr.docId from DocumentReceive dr where dr.receiveId = :userId and dr.type = :type and dr.active = true and dr.clientId = :clientId)"
            + "  or ((:lead = true) and d.id in (select dr2.docId from DocumentReceive dr2 where dr2.receiveId = :orgId and dr2.type = :typeOrg and dr2.active = true and dr2.clientId=:clientId))"
            + "  or (d.id in (select do0.id from DocumentOut do0 where do0.createBy=:userId and do0.active = true and do0.clientId =:clientId))"
            + "  or ((:vanThu = true) and d.id in (select do1.id from DocumentOut do1 where (do1.status = :choBanHanh or do1.status = :daBanHanh) and do1.active = true and do1.clientId = :clientId "
            + " AND ((:clericalOrg is true AND do1.orgIssuedId = c.orgId) OR (:clericalOrg is false AND do1.orgIssuedId = :orgId)))) "
            + ")")
    List<DocumentOut> findByUserIdAndClientId(boolean clericalOrg, DocumentStatusEnum[] docStatusId,
            boolean lead, Long orgId, String typeOrg, String type, Long userId,
            boolean vanThu, DocumentStatusEnum choBanHanh, DocumentStatusEnum daBanHanh, Long clientId);

    List<DocumentOut> findByClientIdAndActiveTrueAndIdIn(Long clientId, List<Long> ids);

    @Query("SELECT count(1) > 0 FROM DocumentOut d WHERE d.numberOrSign = :numberOrSign AND  ((:bookId) is null or d.bookId = (:bookId)) "
            +
            " AND d.clientId = :clientId AND d.active is TRUE AND d.status not in (:statusNoCheck) ")
    boolean checkNumberOrSign(String numberOrSign, Long bookId, Long clientId, DocumentStatusEnum statusNoCheck);

    @Query("SELECT DISTINCT d.address FROM OutsideReceiveDocument d WHERE d.clientId = :clientId AND d.active is TRUE AND d.address IS NOT NULL ORDER BY d.address ")
    List<String> getOutsideReceiveList(Long clientId);

    @Query(name = "DocumentOutProcess.signer", nativeQuery = true)
    List<KnowableDto> signerDocumentOut(Long userId, Long clientId);

    @Query("SELECT new com.vz.backend.business.dto.UserConditionDto(u.id, u.positionModel.isDefault, u.positionModel.isBreadth, u.positionModel.isSiblings, u.org, u.positionModel.id, u.positionModel.name, u.positionModel.order, u.fullName, u.userName, u.orgModel.name) "
            + "FROM User u JOIN AuthorityUser a ON u.id = a.userId AND a.clientId = u.clientId AND a.active = u.active "
            + "WHERE a.active IS TRUE AND a.authority = :authority AND a.clientId = :clientId AND u.id != :userId")
    List<UserConditionDto> findListUserByAuthority(Long clientId, Long userId, AuthorityEnum authority);

    @Query("SELECT new com.vz.backend.business.dto.UserConditionDto(u.id, u.positionModel.isDefault, u.positionModel.isBreadth, u.positionModel.isSiblings, u.org, u.positionModel.id, u.positionModel.name, u.positionModel.order, u.fullName, u.userName, u.orgModel.name) "
            + "FROM User u "
            + "WHERE u.org IN (:orgIds) AND u.active = true AND u.clientId = :clientId AND u.id != :userId "
            + "ORDER BY u.lead DESC")
    List<UserConditionDto> findListUserByOrg(Long clientId, Long userId, List<Long> orgIds);

    @Query("SELECT new com.vz.backend.business.dto.UserConditionDto(u.id, u.positionModel.isDefault, u.positionModel.isBreadth, u.positionModel.isSiblings, u.org, u.positionModel.id, u.positionModel.name, u.positionModel.order, u.fullName, u.userName, u.orgModel.name) "
            + "FROM User u "
            + "WHERE u.org IN (:orgIds) AND u.active = true AND u.clientId = :clientId AND u.id != :userId AND lower(u.positionModel.name) LIKE %:name% "
            + "ORDER BY u.lead DESC")
    List<UserConditionDto> findListCanBo(Long clientId, Long userId, List<Long> orgIds, String name);

    @Query("SELECT new com.vz.backend.business.dto.UserConditionDto(u.id, u.positionModel.isDefault, u.positionModel.isBreadth, u.positionModel.isSiblings, u.org, u.positionModel.id, u.positionModel.name, u.positionModel.order, u.fullName, u.userName, u.orgModel.name) "
            + "FROM User u "
            + "WHERE u.org = :orgId AND u.active IS TRUE AND u.clientId = :clientId AND u.id != :userId AND u.orgModel.active IS TRUE")
    List<UserConditionDto> findListUserByOrgId(Long clientId, Long orgId, Long userId);

    @Query("SELECT DISTINCT new com.vz.backend.business.dto.UserConditionDto(u.id, u.positionModel.isDefault, u.positionModel.isBreadth, u.positionModel.isSiblings, u.org, u.positionModel.id, u.positionModel.name, u.positionModel.order, u.fullName, u.userName, u.orgModel.name) "
            + "FROM User u LEFT JOIN AuthorityUser a ON a.userId = u.id "
            + "WHERE u.orgModel.parentId = :orgId AND u.active = true AND u.clientId = :clientId AND u.id != :userId AND u.orgModel.active IS TRUE "
            + "AND (u.lead IS TRUE OR a.authority = :authority) ")
    List<UserConditionDto> findByClientIdAndParentIdAndAuthority(Long clientId, Long userId, Long orgId,
            AuthorityEnum authority);

    @Query(name = "DocumentOutProcess.signerId", nativeQuery = true)
    List<SignerDto> getSignerIdByCreator(String text, Long userId, Long clientId);

    @Query(value = "select d from Documents d where d.docOutId=:docOutId and d.active=true and d.clientId=:clientId")
    Documents findByDocumentsAndActiveAndDocOutId(Long docOutId, Long clientId);

    @Query(value = "select d from Documents d where d.docOutId=:docOutId and d.active=true and d.clientId=:clientId")
    List<Documents> findByListDocumentsAndActiveAndDocOutId(Long docOutId, Long clientId);

    @Query(value = "select d from DocumentOut d INNER JOIN DocumentReceive dr ON d.id = dr.docId "
            + " LEFT JOIN DocumentUser du on du.docId = d.id and du.userId in(:userIds) and d.active = true and du.docType = :docType  "
            + " LEFT JOIN ClericalOrg c on c.userId in(:userIds) and c.active= true where d.status = :status and d.orgIssuedId = c.orgId "
            + " and ((:bookId) is null or d.bookId = (:bookId))"
            + " and ((:numberOrSign) is null or lower(d.numberOrSign) like %:numberOrSign%) "
            + " and ((:preview) is null or lower(d.preview) like %:preview%) "
            + " and ((:docStatusId) is null or d.status = (:docStatusId)) "
            + " and ((:urgentId) is null or d.urgentId = (:urgentId)) "
            + " and ((:securityId) is null or d.securityId = (:securityId)) "
            + " and ((:docFieldsId) is null or d.docFieldId = (:docFieldsId)) "
            + " AND (:orgIssuedId is null OR d.orgIssuedId = :orgIssuedId) "
            + " AND (:orgReceiveId is null OR dr.receiveId = :orgReceiveId) "
            + " AND (coalesce(:createFrom, null) is null or d.createDate >= :createFrom) "
            + " AND (coalesce(:createTo, null) is null or d.createDate <= :createTo) "
            // + " AND (:docTypeId is null OR d.docTypeId = :docTypeId) "
            + " and (d.clientId = :clientId ) ")
    List<DocumentOut> getFollowDocumentVTBBanHanh(String numberOrSign, Long bookId,
            String preview, Long docFieldsId,
            DocumentStatusEnum docStatusId, Long urgentId, Long orgIssuedId, Long orgReceiveId, Date createFrom,
            Date createTo,
            Long securityId, Long clientId, List<Long> userIds, DocumentTypeEnum docType, DocumentStatusEnum status);

    @Query("SELECT d FROM DocumentOut d INNER JOIN DocumentReceive dr ON d.id = dr.docId LEFT JOIN ObjectRead ob ON ob.objId = d.id AND ob.userId in (:userId) AND ob.type = :docType "
            + "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId in (:userId) AND du.docType = :docType "
            + "LEFT JOIN DocumentReceive dr ON dr.docId = d.id AND dr.receiveId in (:userId) "
            + " where ((:numberOrSign) is null or lower(d.numberOrSign) like %:numberOrSign%) "
            + " and ((:bookId) is null or d.bookId = (:bookId))"
            + " and ((:preview) is null or lower(d.preview) like %:preview%) "
            + " and ((:docStatusId) is null or d.status = (:docStatusId)) "
            + " and ((:urgentId) is null or d.urgentId = (:urgentId)) "
            + " and ((:securityId) is null or d.securityId = (:securityId)) "
            + " and ((:docFieldsId) is null or d.docFieldId = (:docFieldsId)) " + " and (d.clientId = :clientId )"
            + " AND (:orgIssuedId is null OR d.orgIssuedId = :orgIssuedId) "
            + " AND (:orgReceiveId is null OR dr.receiveId = :orgReceiveId) "
            + " AND (coalesce(:createFrom, null) is null or d.createDate >= :createFrom) "
            + " AND (coalesce(:createTo, null) is null or d.createDate <= :createTo) "
            // + " AND (:docTypeId is null OR d.docTypeId = :docTypeId) "
            + " and d.active = true and dr.status= :status ")
    List<DocumentOut> getFollowDocumentLeadBan(String numberOrSign, Long bookId,
            String preview, Long docFieldsId,
            DocumentStatusEnum docStatusId, Long urgentId,
            Long orgIssuedId, Long orgReceiveId, Date createFrom, Date createTo,
            Long securityId, Long clientId,
            List<Long> userId, DocumentTypeEnum docType, DocumentStatusEnum status);

    @Query(value = "SELECT d.id as id FROM vz.DOCUMENT_OUT d " +
            "INNER JOIN vz.SYS_USER u3 ON u3.id = :userId " +
            "INNER JOIN vz.SYS_USER u2 ON u2.id = d.person_enter_id " +
            "WHERE d.active = TRUE  AND d.client_id = :clientId AND d.status = 'DA_BAN_HANH' AND ( " +
            "(d.list_signers_name IS NOT NULL AND u3.full_name IN (SELECT TRIM(UNNEST(string_to_array(d.list_signers_name, ','))))) "
            +
            "OR (d.list_signers_id IS NOT NULL AND u3.id IN (SELECT CAST(TRIM(UNNEST(string_to_array(d.list_signers_id, ','))) AS INTEGER)))) "
            +
            "AND (:preview IS NULL OR LOWER(d.preview) like LOWER(CONCAT('%', CAST(:preview AS TEXT), '%'))) " +
            "AND (:numberOrSign IS NULL OR LOWER(d.number_sign) like LOWER(CONCAT('%', CAST(:numberOrSign AS TEXT), '%'))) "
            +
            "AND (:orgName IS NULL OR LOWER(d.org_create_name) like LOWER(CONCAT('%', CAST(:orgName AS TEXT), '%'))) " +
            "AND (:docFieldId = 0 OR d.field_id = :docFieldId) " +
            "AND (:docTypeId = 0 OR d.doctype_id =:docTypeId) " +
            "AND (:personEnter IS NULL OR LOWER(u2.user_name) = LOWER(CONCAT(CAST(:personEnter AS TEXT), '')) OR LOWER(u2.full_name) LIKE LOWER(CONCAT('%', CAST(:personEnter AS TEXT), '%')))"
            +
            "AND (coalesce(:startDate, null) IS NULL OR  d.create_date > :startDateTime) " +
            "AND (coalesce(:endDate, null) IS NULL OR d.create_date < :endDateTime) " +
            "AND (coalesce(:startIssued, null) IS NULL OR d.issued_date > :startIssuedTime) " +
            "AND (coalesce(:endIssued, null) IS NULL OR d.issued_date < :endIssuedTime) ", nativeQuery = true)
    List<Long> signerDocumentOutSearch(Long userId, Long clientId, String preview, String numberOrSign, String orgName,
            Long docFieldId,
            Long docTypeId, String personEnter, Date startDate, Date endDate, Date startIssued, Date endIssued,
            Date startDateTime, Date endDateTime, Date startIssuedTime, Date endIssuedTime);

    @Query(value = "select d from DocumentOut d  where d.id  in (:docIds) and d.updateBy =:vanThuBanId order by d.createDate DESC")
    List<DocumentOut> findAllDocByIdsNotPaging(List<Long> docIds, Long vanThuBanId);

    @Query(value = "select d from DocumentOut d "
            + " where ((:numberOrSign) is null or lower(d.numberOrSign) like %:numberOrSign%) "
            + " and ((:preview) is null or lower(d.preview) like %:preview%) "
            + " and ((:bookId) is null or d.bookId = (:bookId))"
            + " and ((:urgentId) is null or d.urgentId = (:urgentId)) "
            + " and ((:securityId) is null or d.securityId = (:securityId)) "
            + " and ((:docFieldsId) is null or d.docFieldId = (:docFieldsId)) " + " and (d.clientId = :clientId )"
            + " AND (:orgIssuedId is null OR d.orgIssuedId = :orgIssuedId) "
            + " and ((:status) is null or d.status = (:status)) "
            + " AND (coalesce(:createFrom, null) is null or d.createDate >= :createFrom) "
            + " AND (coalesce(:createTo, null) is null or d.createDate <= :createTo) "
            + " AND (:docTypeId is null OR d.docTypeId = :docTypeId) "
            + " and d.active = true "
            + " and d.orgIssuedId in (:orgIds)")
    List<DocumentOut> exportAllDoc(String numberOrSign,
            String preview, Date createFrom, Date createTo, Long docTypeId, Long docFieldsId, Long clientId,
            List<Long> orgIds, DocumentStatusEnum status, Long bookId, Long urgentId, Long securityId,
            Long orgIssuedId);

    @Query(value = "select d from DocumentOut d"
            + " LEFT JOIN DocumentUser du on du.docId = d.id and du.userId =:userId and d.active = true and du.docType = :docType  "
            + " LEFT JOIN ClericalOrg c on c.userId = :userId and c.active= true where  d.orgIssuedId = c.orgId "
            + " and ((:bookId) is null or d.bookId = (:bookId))"
            + " and ((:numberOrSign) is null or lower(d.numberOrSign) like %:numberOrSign%) "
            + " and ((:preview) is null or lower(d.preview) like %:preview%) "
            + " and ((:status) is null or d.status = (:status)) "
            + " and ((:urgentId) is null or d.urgentId = (:urgentId)) "
            + " and ((:securityId) is null or d.securityId = (:securityId)) "
            + " and ((:docFieldsId) is null or d.docFieldId = (:docFieldsId)) "
            + " AND (:orgIssuedId is null OR d.orgIssuedId = :orgIssuedId) "
            + " AND (coalesce(:createFrom, null) is null or d.createDate >= :createFrom) "
            + " AND (coalesce(:createTo, null) is null or d.createDate <= :createTo) "
            + " AND (:docTypeId is null OR d.docTypeId = :docTypeId) "
            + " and (d.clientId = :clientId ) ")
    List<DocumentOut> exportFollowDocumentVTBBanHanh(String numberOrSign, String preview, Date createFrom,
            Date createTo, Long docFieldsId,
            Long clientId, Long userId, DocumentTypeEnum docType, DocumentStatusEnum status, Long bookId, Long urgentId,
            Long securityId, Long orgIssuedId, Long docTypeId);

    @Query("SELECT d FROM DocumentOut d LEFT JOIN ObjectRead ob ON ob.objId = d.id AND ob.userId in (:userId) AND ob.type = :docType "
            + "LEFT JOIN DocumentUser du ON du.docId = d.id AND du.userId in (:userId) AND du.docType = :docType "
            + "LEFT JOIN DocumentReceive dr ON dr.docId = d.id AND dr.receiveId in (:userId) "
            + " where ((:numberOrSign) is null or lower(d.numberOrSign) like %:numberOrSign%) "
            + " and ((:bookId) is null or d.bookId = (:bookId))"
            + " and ((:preview) is null or lower(d.preview) like %:preview%) "
            + " and ((:status) is null or d.status = (:status)) "
            + " and ((:urgentId) is null or d.urgentId = (:urgentId)) "
            + " and ((:securityId) is null or d.securityId = (:securityId)) "
            + " and ((:docFieldsId) is null or d.docFieldId = (:docFieldsId)) " + " and (d.clientId = :clientId )"
            + " AND (:orgIssuedId is null OR d.orgIssuedId = :orgIssuedId) "
            + " AND (coalesce(:createFrom, null) is null or d.createDate >= :createFrom) "
            + " AND (coalesce(:createTo, null) is null or d.createDate <= :createTo) "
            + " AND (:docTypeId is null OR d.docTypeId = :docTypeId) "
            + " and d.active = true and dr.status= :status ")
    List<DocumentOut> exportFollowDocumentLeadBan(String numberOrSign,
            String preview, Date createFrom, Date createTo, Long docFieldsId, Long clientId,
            List<Long> userId, DocumentTypeEnum docType, DocumentStatusEnum status, Long bookId, Long urgentId,
            Long securityId, Long orgIssuedId, Long docTypeId);
}
