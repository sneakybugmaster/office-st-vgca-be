package com.vz.backend.business.repository;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.Delegate;
import com.vz.backend.business.dto.DelegateDto;
import com.vz.backend.business.dto.DocumentProcessDto;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.business.dto.UserDelegateDto;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDelegateRepository extends IRepository<Delegate> {

	@Query("SELECT count(*)>0 FROM Delegate d WHERE d.fromUserId = :fromUser AND d.toUserId = :toUser AND d.startDate <= :date AND d.endDate >= :date")
	boolean existFromUserByToUser(Long fromUser, Long toUser, Date date);

	@Query("select d.fromUserId from Delegate d where d.toUserId =:toUserId and d.active=true and d.startDate <= :date and d.endDate >= :date group by d.fromUserId ")
	List<Long> getListFrUserByToUser(Long toUserId, Date date);

	@Query("select d.toUserId from Delegate d where d.fromUserId = (:frUsers) and d.active=true and d.clientId=:clientId and d.startDate <= :date and d.endDate >= :date group by d.toUserId ")
	List<Long> getListToUserByFrUser(Long frUsers, Date date, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.DocumentProcessDto( du.important, p.id,  "
			+ " p.user.id, p.user.userName, p.user.fullName, p.updateDate, p.documentOut, "
			+ " p.handleStatus,  p.read) " + "FROM DocumentOutProcess p "
			+ "LEFT JOIN DocumentUser du ON du.docId = p.docId AND du.userId = :userId AND du.docType = :docType "
			+ "WHERE p.active=TRUE AND (p.delegate.toUserId = :userId OR p.delegate.fromUserId =:userId) AND p.delegate.startDate <= :now AND :now <= p.delegate.endDate AND p.handleStatus IN :status AND p.documentOut.status IN (:docStatus)")
	Page<DocumentProcessDto> findDelegateByUserIdAndHandleStatusAndDocStatus(DocumentTypeEnum docType, Long userId,
			Date now, DocumentOutHandleStatusEnum[] status, DocumentStatusEnum[] docStatus, Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.DocumentProcessDto( du.important, p.id,  "
			+ " p.user.id, p.user.userName, p.user.fullName, p.updateDate, p.documentOut, "
			+ " p.handleStatus,  p.read) " + "FROM DocumentOutProcess p "
			+ "LEFT JOIN DocumentUser du ON du.docId = p.docId AND du.userId = :userId AND du.docType = :docType "
			+ "WHERE p.active=TRUE AND (p.delegate.toUserId = :userId OR p.delegate.fromUserId =:userId) AND p.delegate.startDate < :now AND :now < p.delegate.endDate AND p.handleStatus IN :status AND p.documentOut.status IN (:docStatus) "
			+ "AND (lower(p.documentOut.preview) LIKE %:text% OR lower(p.documentOut.numberOrSign) like %:text% OR lower(p.documentOut.userEnter.userName) like %:text%)")
	Page<DocumentProcessDto> search(DocumentTypeEnum docType, Long userId, Date now,
			DocumentOutHandleStatusEnum[] status, String text, DocumentStatusEnum[] docStatus, Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.DocumentProcessDto( du.important, p.id,  "
			+ " p.user.id, p.user.userName, p.user.fullName, p.updateDate, p.documentOut, "
			+ " p.handleStatus,  p.read) " + "FROM DocumentOutProcess p "
			+ "LEFT JOIN DocumentUser du ON du.docId = p.docId AND du.userId = :userId AND du.docType = :docType "
			+ "WHERE p.active=TRUE AND p.delegate.toUserId = :userId AND p.delegate.startDate < :now AND :now < p.delegate.endDate AND p.handleStatus IN :status AND p.documentOut.status IN (:docStatus) "
			+ "AND (:important is null OR (:important = false AND du.important is null) OR du.important = :important) "
			+ "AND (:preview IS NULL OR lower(p.documentOut.preview) LIKE %:preview%)"
			+ "AND (:numberOrSign IS NULL OR lower(p.documentOut.numberOrSign) LIKE %:numberOrSign%)"
			+ "AND (:orgName IS NULL OR lower(p.orgName) LIKE %:orgName%)"
			+ "AND (:docTypeId IS NULL OR p.documentOut.docTypeId = :docTypeId)"
			+ "AND (:docFieldId IS NULL OR p.documentOut.docFieldId = :docFieldId)"
			+ "AND (:personEnter IS NULL OR LOWER(p.documentOut.userEnter.userName) = :personEnter OR LOWER(p.documentOut.userEnter.fullName) LIKE '%'||:personEnter||'%')"
			+ "AND (coalesce(:startDate, null) IS NULL OR p.documentOut.createDate > :startDate)"
			+ "AND (coalesce(:endDate, null) IS NULL OR p.documentOut.createDate < :endDate) ")
	Page<DocumentProcessDto> searchAdvance(Boolean important, DocumentTypeEnum docType, Long userId, Date now,
			DocumentOutHandleStatusEnum[] status, String preview, String numberOrSign, Long docTypeId, Long docFieldId,
			String orgName, String personEnter, Date startDate, Date endDate, DocumentStatusEnum[] docStatus,
			Pageable pageable);

	@Query("SELECT new com.vz.backend.business.dto.DelegateDto(de.id, de.numberOrSign, de.fromUserId, de.fromUser.fullName, de.toUserId, de.toUser.fullName, de.startDate, de.endDate, de.active) "
			+ "FROM Delegate de WHERE de.fromUser.org=:yourOrg AND (:active is null or de.active = :active) OR de.createBy=:yourId")
	Page<DelegateDto> list(Long yourOrg, Long yourId, Pageable pageable, Boolean active);

	@Query("SELECT new com.vz.backend.business.dto.DelegateDto(de.id, de.numberOrSign, de.fromUserId, de.fromUser.fullName, de.toUserId, de.toUser.fullName, de.startDate, de.endDate) "
			+ "FROM Delegate de WHERE de.fromUser.org=:yourOrg AND de.active is TRUE AND("
			+ ":text is NULL OR LOWER(de.numberOrSign) LIKE '%'||:text||'%' OR LOWER(de.fromUser.userName)=:text OR LOWER(de.fromUser.fullName) LIKE '%'||:text||'%' "
			+ " OR LOWER(de.toUser.userName)=:text OR LOWER(de.toUser.fullName) LIKE '%'||:text||'%')")
	Page<DelegateDto> quickSearch(String text, Long yourOrg, Pageable pageable);

    @Query("SELECT new com.vz.backend.business.dto.DelegateDto(de.id, de.numberOrSign, de.fromUserId, de.fromUser.fullName, de.toUserId, de.toUser.fullName, de.startDate, de.endDate) "
            + "FROM Delegate de WHERE de.fromUser.org=:yourOrg AND de.active is TRUE "
            + "AND (:numberOrSign is NULL OR LOWER(de.numberOrSign) LIKE %:numberOrSign%)"
            + "AND (:fromUser is NULL OR LOWER(de.fromUser.userName)=:fromUser OR LOWER(de.fromUser.fullName) LIKE '%'||:fromUser||'%')"
            + "AND (:toUser is NULL OR LOWER(de.toUser.userName)=:toUser OR LOWER(de.toUser.fullName) LIKE '%'||:toUser||'%')"
            + "AND (coalesce(:startDate, null) is NULL OR de.startDate > :startDate) AND (coalesce(:endDate, null) is NULL OR de.endDate < :endDate) "
			+ "OR de.createBy=:yourId")
    Page<DelegateDto> search(String numberOrSign, String fromUser, String toUser, Date startDate, Date endDate,
            Long yourOrg, Long yourId, Pageable pageable);

    @Query("SELECT new com.vz.backend.business.dto.DelegateDto(de.id, de.numberOrSign, de.fromUserId, de.fromUser.fullName, de.toUserId, de.toUser.fullName, de.startDate, de.endDate) "
            + "FROM Delegate de WHERE de.active is TRUE "
            + "AND de.fromUserId = :fromUserId "
            + "AND de.toUserId = :toUserId "
            + "AND de.startDate < :endDate AND de.endDate > :startDate")
    List<DelegateDto> exist(Long fromUserId, Long toUserId, Date startDate, Date endDate);

	@Query("SELECT new com.vz.backend.business.dto.UserDelegateDto(de.id, de.createDate, de.endDate, de.fromUserId, de.toUser.id, de.toUser.userName, de.toUser.fullName, de.toUser.positionModel.name, de.toUser.positionModel.order) "
			+ "FROM Delegate de WHERE de.startDate < :now and :now < de.endDate and de.fromUserId IN :fromIds AND de.active is TRUE")
	List<UserDelegateDto> getDelegateByIds(Set<Long> fromIds, Date now);

	@Query("select d.toUserId from Delegate d where d.id = :delegateId and d.active=true and d.startDate <= :date and d.endDate >= :date")
	Long findByIdAndDate(Long delegateId, Date date);
	
	@Query("SELECT NEW com.vz.backend.business.dto.ReportDocByTypeDto('DOC_OUT_DELEGATE', COUNT(DISTINCT p.docId))"
			+ " FROM DocumentOutProcess p "
			+ " LEFT JOIN DocumentUser du ON du.docId = p.docId AND du.userId = :userId AND du.docType = 'VAN_BAN_DI' "
			+ " WHERE p.active=TRUE AND p.delegate.toUserId = :userId AND p.delegate.startDate < :now AND :now < p.delegate.endDate "
			+ " AND p.handleStatus IN :status AND p.documentOut.status IN (:docStatus) AND p.clientId=:clientId AND p.documentOut.active = TRUE")
	ReportDocByTypeDto reportDocDelegate(Long userId, Date now, List<DocumentOutHandleStatusEnum> status,
			List<DocumentStatusEnum> docStatus, Long clientId);
}
