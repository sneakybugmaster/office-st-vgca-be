package com.vz.backend.business.repository;

import java.util.Date;
import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentInProcess;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.business.dto.UserConditionDto;
import com.vz.backend.business.dto.fullreport.SimpleProcessIn;
import com.vz.backend.business.dto.kpi.KPIDataDto;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentInProcessRepository extends IRepository<DocumentInProcess> {

	@Query("select d from DocumentInProcess d where d.docId = :docId and d.clientId =:clientId and d.active = true")
	List<DocumentInProcess> getListProcessByDocId(Long docId, Long clientId);

	List<DocumentInProcess> findByDocIdAndStepAndClientIdAndActive(Long docId, Integer step, Long clientId,
			boolean active);

	List<DocumentInProcess> findByDocIdAndStepAndClientIdAndActiveAndHandleType(Long docId, Integer step, Long clientId,
			boolean active, HandleTypeEnum type);

	@Query("select d from DocumentInProcess d "
			+ " where d.docId = :docId and d.clientId =:clientId and d.active = true and d.toUser = :userId "
			+ " and d.step in (select max(d1.step) from DocumentInProcess d1 where d1.docId = :docId "
			+ "and d1.clientId =:clientId and d1.active = true and d1.toUser = :userId group by d1.toUser order by d.id)")
	List<DocumentInProcess> findByToUserAndDocId(Long docId, Long userId, Long clientId);

	@Query("select d from DocumentInProcess d where d.docId = :docId and d.clientId =:clientId and d.active = true and d.step = 1")
	List<DocumentInProcess> findByDocIdAndFirstStep(Long docId, Long clientId);

	@Query("select count(distinct p.docId) from DocumentInProcess p where p.toUser = :toUser"
			+ " and p.active = true and p.clientId = :clientId and p.handleStatus in (:handleStatus)"
			+ " and p.document.status in (:docStatusId)"
			+ " and (p.docId, p.step) in (select sp.docId, max(sp.step) from DocumentInProcess sp "
			+ "where sp.clientId = :clientId and sp.active = true and ((:toUser) is null or sp.toUser = (:toUser)) and p.handleStatus in (:handleStatus) group by sp.docId)")
	Long countDocByUser(Long clientId, Long toUser, List<DocumentInHandleStatusEnum> handleStatus,
			List<DocumentStatusEnum> docStatusId);
	
	@Query("select count(distinct p.docId) from DocumentInProcess p where p.toUser = :toUser"
			+ " and p.active = true and p.clientId = :clientId and p.handleStatus in (:handleStatus)"
			+ " and p.document.status in (:docStatusId) AND p.handleType = :handleType "
			+ " and (p.docId, p.step) in (select sp.docId, max(sp.step) from DocumentInProcess sp "
			+ "where sp.clientId = :clientId and sp.active = true and ((:toUser) is null or sp.toUser = (:toUser)) and p.handleStatus in (:handleStatus) group by sp.docId)")
	Long countDocByUser(Long clientId, Long toUser, HandleTypeEnum handleType, List<DocumentInHandleStatusEnum> handleStatus,
			List<DocumentStatusEnum> docStatusId);

	@Query("select count(distinct p.docId) from DocumentInProcess p where p.toUser = :toUser"
			+ " and p.active = true and p.clientId = :clientId and p.handleStatus not in (:handleStatus)"
			+ " and p.document.status in (:docStatusId) and p.deadline < current_date()"
			+ " and (p.docId, p.step) in (select sp.docId, max(sp.step) from DocumentInProcess sp "
			+ "where sp.clientId = :clientId and sp.active = true and ((:toUser) is null or sp.toUser = (:toUser)) group by sp.docId)")
	Long countDocByUserAndOverDue(Long clientId, Long toUser, List<DocumentInHandleStatusEnum> handleStatus,
			List<DocumentStatusEnum> docStatusId);

	@Query("SELECT new com.vz.backend.business.dto.fullreport.SimpleProcessIn(p.id, p.docId, p.updateDate, p.handleType, p.handleStatus, p.deadline) FROM DocumentInProcess p "
			+ "WHERE p.active is TRUE AND p.toUser = :userId AND p.clientId = :clientId AND "
			+ "(coalesce(:startDate, null) is NULL or p.updateDate BETWEEN :startDate and :endDate)")
	List<SimpleProcessIn> fullReport(Long userId, Long clientId, Date startDate, Date endDate);

	@Query("select d from DocumentInProcess d where d.docId = (:docId) and d.clientId =:clientId "
			+ " and d.active = true and d.step =:step and (d.toUser in (:idUs) OR d.delegaterId in (:idUs))")
	List<DocumentInProcess> findByDocIdAndStepAndToUsers(Long docId, Integer step, List<Long> idUs, Long clientId);

	@Query("select d from DocumentInProcess d where d.docId = (:docId) and d.clientId =:clientId "
			+ " and d.active = true and d.step =:step and d.toUser = :toUser and d.handleType = :handleType")
	DocumentInProcess findByDocIdAndStepAndHandleTypeAndToUser(Long docId, Integer step, Long toUser,
			HandleTypeEnum handleType, Long clientId);

	@Query("select d from DocumentInProcess d where d.docId = (:docId) and d.clientId =:clientId "
			+ " and d.active = true and (d.toUser = :toUser OR d.delegaterId = :toUser)"
			+ " and d.handleType = :handleType AND d.handleStatus = :handleStatus "
			+ "ORDER BY STEP DESC")
	List<DocumentInProcess> findByDocIdAndHandleTypeAndToUserOrDelegaterAndHandleStatus(Long docId, Long toUser,
			HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus, Long clientId);

	@Query("select d from DocumentInProcess d where d.docId = (:docId) and d.clientId =:clientId "
			+ " and d.active = true and d.step =:step and d.delegaterId = :delegaterId"
			+ " and d.handleType = :handleType")
	DocumentInProcess findByDocIdAndStepAndHandleTypeAndDelegaterId(Long docId, Integer step, Long delegaterId,
			HandleTypeEnum handleType, Long clientId);

	// lấy max
	@Query("select d from DocumentInProcess d "
			+ " where d.docId in(:docIdList) and d.clientId =:clientId and d.active = true"
			+ " and (d.docId, d.step) in (select d1.docId, max(d1.step) from DocumentInProcess d1 where d1.docId in(:docIdList) and d1.clientId =:clientId and d1.active = true group by d1.docId)")
	List<DocumentInProcess> findByDocIdAndLastedStep(Long clientId, List<Long> docIdList);

	@Query("select d from DocumentInProcess d "
			+ " where d.docId in(:docIdList) and d.clientId =:clientId and d.active = true and d.handleType =:type "
			+ " and (d.docId, d.step) in (select d1.docId, max(d1.step) from DocumentInProcess d1 where d1.docId in(:docIdList) and d1.clientId =:clientId and d1.active = true group by d1.docId)")
	List<DocumentInProcess> findByDocIdAndLastedStep(Long clientId, List<Long> docIdList, HandleTypeEnum type);

	@Query("select d from DocumentInProcess d "
			+ " where d.docId =:docId and d.clientId =:clientId and d.active = true and d.toUser in (:toUser)"
			+ " and (d.docId, d.step) in (select d1.docId, max(d1.step) from DocumentInProcess d1 "
			+ "where d1.docId =:docId and d1.clientId =:clientId and d1.active = true and d.toUser in (:toUser) group by d1.docId)")
	List<DocumentInProcess> findLastProcessByDocId(Long docId, Long clientId, List<Long> toUser);

	@Query("select d from DocumentInProcess d where d.docId = (:docId) and d.clientId =:clientId "
			+ " and d.active = true and d.step =:step and d.frUser = :frUser and d.node = :node"
			+ " and d.handleType = :main")
	List<DocumentInProcess> findByDocIdAndStepAndFrUserId(Long docId, Integer step, Long frUser, HandleTypeEnum main,
			Long node, Long clientId);

	@Query("select d from DocumentInProcess d "
			+ " where d.docId = :docId and d.clientId =:clientId and d.active = true and d.delegaterId = :userId "
			+ " and d.step in (select max(d1.step) from DocumentInProcess d1 "
			+ "where d1.docId = :docId and d1.clientId =:clientId and d1.active = true and d1.delegaterId = :userId group by d1.toUser order by d.id desc)")
	List<DocumentInProcess> findByDelegateAndDocId(Long docId, Long userId, Long clientId);

	@Query("select d from DocumentInProcess d "
			+ " where d.docId = :docId and d.clientId =:clientId and d.active = true and (d.delegaterId = :userId or d.toUser = :userId) "
			+ " and d.step in (select max(d1.step) from DocumentInProcess d1 "
			+ "where d1.docId = :docId and d1.clientId =:clientId and d1.active = true and (d1.delegaterId = :userId or d1.toUser = :userId))")
	List<DocumentInProcess> findByRelatedAndDocId(Long docId, Long userId, Long clientId);

	DocumentInProcess findFirstByDocIdAndHandleStatusAndActive(Long parentId, DocumentInHandleStatusEnum chuyenDonVi,
			boolean b);

	DocumentInProcess findFirstByDocIdAndHandleTypeAndActiveOrderByStepDesc(Long docId, HandleTypeEnum main, boolean active);

	DocumentInProcess findByDocIdAndStepAndHandleTypeAndReviewAndActive(Long docId, int step, HandleTypeEnum handleType, boolean review, boolean active);

	@Query("SELECT p.toUser FROM DocumentInProcess p WHERE p.docId =:docId AND (p.toUser IN (:userIds) OR p.delegaterId IN (:userIds)) AND p.clientId = :clientId AND p.active = true")
	List<Long> findByRelatedAndDocId(List<Long> userIds, Long docId, Long clientId);

	DocumentInProcess findFistByDelegaterIdAndDocIdAndHandleTypeAndHandleStatusAndActiveAndClientIdOrderByIdDesc(
			Long delegateId, Long docId, HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus, boolean active,
			Long clientId);

	DocumentInProcess findFirstByDocIdAndToUserAndHandleTypeAndHandleStatusInAndActiveAndClientIdOrderByStepDesc(Long docId,
			Long userId, HandleTypeEnum show, List<DocumentInHandleStatusEnum> asList, boolean active, Long clientId);

	DocumentInProcess findFistByToUserAndDocIdAndHandleTypeAndHandleStatusAndActiveAndClientIdOrderByIdDesc(Long userId,
			Long docId, HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus, boolean active, Long clientId);

	@Query("select d from DocumentInProcess d "
			+ " where d.docId = :docId and d.clientId =:clientId and d.active = true and d.handleType = :type and d.handleStatus in (:status)"
			+ " and d.step in (select max(d2.step) from DocumentInProcess d2 "
			+ "where d2.docId = :docId and d2.handleType = :type and d2.handleStatus in (:status) and d2.clientId =:clientId and d2.active = true and d2.transferStep > 0)")
	List<DocumentInProcess> getTransferStep(Long docId, HandleTypeEnum type, DocumentInHandleStatusEnum[] status,Long clientId);
	
	@Query("SELECT d FROM DocumentInProcess d "
			+ " WHERE d.docId = :docId AND d.clientId =:clientId AND d.active IS TRUE AND d.toUser = :userId AND d.handleStatus = 'DA_XU_LY'"
			+ " AND d.step in (SELECT MAX(d2.step) FROM DocumentInProcess d2 "
			+ " WHERE d2.docId = :docId AND d2.handleStatus = 'DA_XU_LY' AND d2.clientId =:clientId AND d2.active IS TRUE AND d2.toUser = :userId AND d2.transferStep > 0)")
	List<DocumentInProcess> transferToUserId(Long docId, Long userId, Long clientId);

	@Query("select max(d1.transferStep) from DocumentInProcess d1 "
			+ "where d1.docId = :docId and d1.handleStatus in (:status) and d1.clientId =:clientId and d1.active = true")
	Integer getMaxTransferStep(Long docId, DocumentInHandleStatusEnum[] status, Long clientId);
	
	@Query(name = "Documents.menu", nativeQuery = true)
	List<ReportDocByTypeDto> reportDocByType(boolean clericalOrg, Long userId, Long orgId, Long clientId);

	@Query("select d from DocumentInProcess d "
			+ " where d.docId = :docId and d.clientId =:clientId and d.active = true and (d.toUser = :userId OR d.delegaterId =:userId )")
	List<DocumentInProcess> findAllByToUserAndDocId(Long docId, Long userId, Long clientId);

	@Query("select d from DocumentInProcess d "
			+ " where d.docId = :docId and d.clientId =:clientId and d.active = true and (d.frUser = :userId OR d.delegaterId = :userId) and d.node = :node"
			+ " and d.step in (select max(d1.step) from DocumentInProcess d1 where d1.docId = :docId and d1.node = :node "
			+ " and d1.clientId =:clientId and d1.active = true and (d1.frUser = :userId OR d1.delegaterId = :userId) group by d1.toUser)")
	List<DocumentInProcess> findByToUserAndDocIdAndNode(Long node, Long docId, Long userId, Long clientId);

	@Query("SELECT p"
			+ " FROM DocumentInProcess p "
			+ " WHERE p.clientId=:clientId AND (:node IS NULL OR p.node=:node) AND (p.step =:step OR p.step = (:step - 1)) AND p.active is true AND p.docId =:docId "
			)
	List<DocumentInProcess> getProcesByNodeIdAndStep(Long docId, Long node, int step, Long clientId);
	
	@Query("SELECT new com.vz.backend.business.dto.UserConditionDto(p.toUser, p.toUsers.positionModel.isDefault, p.toUsers.positionModel.isBreadth, p.toUsers.positionModel.isSiblings, p.toUsers.org, p.toUsers.positionModel.id, p.toUsers.positionModel.name, p.toUsers.positionModel.order, p.toUsers.fullName, p.toUsers.userName, p.toUsers.org, p.toUsers.position, p.toUser) "
			+ " FROM DocumentInProcess p "
			+ " WHERE p.clientId=:clientId AND (:node IS NULL OR p.node=:node) AND p.active is true AND p.docId =:docId "
			)
	List<UserConditionDto> getUserByNodeId(Long docId, Long node, Long clientId);

	@Query("select d from DocumentInProcess d "
			+ " where d.docId = :docId and d.clientId =:clientId and d.active = true and d.node = :node and d.handleType=:type "
			+ " and d.step in (select max(d1.step) from DocumentInProcess d1 where d1.docId = :docId and d1.node = :node and d1.handleType=:type "
			+ " and d1.clientId =:clientId and d1.active = true)")
	List<DocumentInProcess> findByDocIdAndNodeAndType(Long docId, Long node, HandleTypeEnum type, Long clientId);

	@Query("select d.handleType from DocumentInProcess d where d.docId = :docId and d.clientId =:clientId and d.active = true and d.toUser = :userId")
	Set<HandleTypeEnum> getHandleTypeByDocId(Long userId, Long docId, Long clientId);

	@Query("select d from DocumentInProcess d "
			+ "where d.docId = :docId and d.clientId =:clientId and d.active = true AND d.handleType = 'MAIN' AND (d.frUser = :userId OR d.delegaterId = :userId )")
	List<DocumentInProcess> getListTransferTo(Long userId, Long docId, Long clientId);
	
	@Query("select d.toUser from DocumentInProcess d "
			+ "where d.docId = :docId and d.clientId =:clientId and d.active = true AND d.handleType = 'MAIN' AND (d.frUser = :userId)")
	List<Long> getListToUserId(Long userId, Long docId, Long clientId);
	
	@Query("select d.toUser from DocumentInProcess d "
			+ "where d.docId = :docId and d.clientId =:clientId and d.active = true AND (d.frUser IN (:listFrUser))")
	List<Long> getListToUserIdByListFromUser(List<Long> listFrUser, Long docId, Long clientId);
	
	@Query("select d from DocumentInProcess d "
			+ "where d.docId = :docId and d.clientId =:clientId and d.active = true AND (d.toUser = :userId OR d.delegaterId = :userId) ")
	List<DocumentInProcess> getListToTransfer(Long userId, Long docId, Long clientId);

	@Query("select d.frUser from DocumentInProcess d "
			+ "where d.docId = :docId and d.clientId =:clientId and d.active = true AND d.handleType = 'MAIN' AND (d.toUser = :userId) ")
	List<Long> getListFromUserId(Long userId, Long docId, Long clientId);

	List<DocumentInProcess> findByToUserAndDocIdAndClientIdAndActive(Long userId, Long docId, Long clientId, boolean active);

	@Query("SELECT p.toUser FROM DocumentInProcess p WHERE p.docId = :docId AND p.frUser = :fromUser AND p.step = :step AND p.clientId = :clientId AND p.active = :active ")
	List<Long> findToUserByFrUserAndDocIdAndStepAndClientIdAndActive(Long fromUser, Long docId, Integer step, Long clientId, boolean active);

	@Query("SELECT p.toUser FROM DocumentInProcess p WHERE p.docId = :docId AND p.frUser = :fromUser AND p.step = :step AND p.clientId = :clientId AND p.active = :active "
			+ "AND p.handleType = :handleType")
	List<Long> findToUserByFrUserAndDocIdAndStepAndHandleTypeInAndClientIdAndActive(Long fromUser, Long docId, Integer step, List<HandleTypeEnum> handleType, Long clientId, boolean active);
	
	@Query("SELECT p.toUsers FROM DocumentInProcess p WHERE p.docId = :docId AND p.frUser = :fromUser AND p.step = :step AND p.clientId = :clientId AND p.active = :active "
			+ "AND p.handleType = :handleType")
	List<User> findToUserObjByFrUserAndDocIdAndStepAndHandleTypeInAndClientIdAndActive(Long fromUser, Long docId, Integer step, List<HandleTypeEnum> handleType, Long clientId, boolean active);

	@Query("SELECT p.toUser FROM DocumentInProcess p WHERE p.docId = :docId AND p.frUser = :fromUser AND p.step = :step AND p.clientId = :clientId AND p.active = :active "
			+ "AND p.handleType = :handleType AND (:leadership is null OR p.toUsers.positionModel.isLeadership = :leadership)")
	List<Long> findToUserByFrUserAndDocIdAndStepAndHandleTypeInAndLeadershipAndClientIdAndActive(Long fromUser, Long docId, Integer step, List<HandleTypeEnum> handleType, Boolean leadership, Long clientId, boolean active);
	
	@Query("SELECT p.toUser FROM DocumentInProcess p WHERE p.docId = :docId AND p.frUser = :fromUser AND p.step = :step AND p.clientId = :clientId AND p.active = :active "
			+ "AND p.toUsers.org = :orgId")
	List<Long> findToUserByFrUserAndDocIdAndStepAndOrgIdAndClientIdAndActive(Long fromUser, Long docId, Integer step, Long orgId, Long clientId, boolean active);
	
	@Query("SELECT p.toUser FROM DocumentInProcess p WHERE p.docId = :docId AND p.frUser = :fromUser AND p.clientId = :clientId AND p.active = :active "
			+ "AND p.toUsers.org = :orgId")
	List<Long> findToUserByFrUserAndDocIdAndOrgIdAndClientIdAndActive(Long fromUser, Long docId, Long orgId, Long clientId, boolean active);

	@Query("SELECT p.toUser FROM DocumentInProcess p WHERE p.docId = :docId AND p.frUser = :userId AND p.clientId = :clientId AND p.active = :active")
	List<Long> findToUserByFromUserAndDocIdAndClientIdAndActive(Long userId, Long docId, Long clientId, boolean active);

	@Query("SELECT p.toUser FROM DocumentInProcess p WHERE p.docId = :docId AND p.frUser IN (:fromUsers) AND p.step > :step "
			+ "AND (:leadership is null OR p.toUsers.positionModel.isLeadership = :leadership) AND p.clientId = :clientId AND p.active = :active")
	List<Long> findToUserByFromUserInAndDocIdAndStepGreaterAndLeadershipAndClientIdAndActive(List<Long> fromUsers,
			Long docId, Integer step, Boolean leadership, Long clientId, boolean active);

	@Query("SELECT p.toUser FROM DocumentInProcess p WHERE p.docId = :docId AND p.toUsers.positionModel.isLeadership = :isLeadership AND p.clientId = :clientId AND p.active = :active")
	List<Long> findToUserByDocIdAndAuthorityAndClientIdAndActive(Long docId, boolean isLeadership, Long clientId, boolean active);
	
	@Query("SELECT new com.vz.backend.business.dto.kpi.KPIDataDto(p) FROM DocumentInProcess p "
			+ " WHERE (p.toUser IN (:userIds) OR p.delegaterId IN (:userIds)) AND p.clientId =:clientId AND p.active = TRUE AND p.document.active = TRUE"
			+ " AND (COALESCE(p.updateDate, p.createDate) IS NULL OR p.updateDate BETWEEN :startDate AND :endDate)")
	List<KPIDataDto> findAllByToUser(List<Long> userIds, Long clientId, Date startDate, Date endDate);
	
	@Query("SELECT new com.vz.backend.business.dto.kpi.KPIDataDto(p) FROM DocumentInProcess p "
			+ " WHERE (p.toUser = (:userId) OR p.delegaterId = (:userId)) AND p.clientId =:clientId AND p.active = TRUE AND p.document.active = TRUE"
			+ " AND (COALESCE(:startDate, NULL) IS NULL OR COALESCE(:endDate, NULL) IS NULL OR COALESCE(p.updateDate, p.createDate) IS NULL OR p.updateDate BETWEEN :startDate AND :endDate)"
			+ " AND (p.docId, p.step) IN ( "
			+ "    SELECT p1.docId, max(p1.step) "
			+ "    FROM DocumentInProcess p1 WHERE (p1.toUser = :userId OR p1.delegaterId = :userId) AND p1.clientId=:clientId AND p1.active=TRUE GROUP BY p1.docId)"
			)
	List<KPIDataDto> findAllByToUser(Long userId, Long clientId, Date startDate, Date endDate);
	
	@Query("SELECT d FROM DocumentInProcess d "
			+ " WHERE d.docId = :docId AND d.clientId =:clientId AND d.active = TRUE "
//			+ " AND (d.toUser != d.frUser) "
			+ " AND (d.toUser = :userId OR d.delegaterId =:userId) "
			+ " AND d.step IN(SELECT MAX(d1.step) FROM DocumentInProcess d1 WHERE d1.docId = :docId "
			+ " AND d1.clientId =:clientId AND d1.active = TRUE AND (d1.toUser = :userId OR d1.delegaterId =:userId) "
//			+ " AND (d1.toUser != d1.frUser) "
			+ "ORDER BY d.id DESC)")
	List<DocumentInProcess> findByToUserAndDocId2(Long docId, Long userId, Long clientId);

	@Query("SELECT d FROM DocumentInProcess d "
			+ " WHERE d.docId = :docId AND d.clientId =:clientId AND d.active = TRUE "
//			+ " AND (d.toUser != d.frUser) "
			+ " AND (d.toUser in (:userId) OR d.delegaterId in (:userId)) "
			+ " AND d.step IN (SELECT MAX(d1.step) FROM DocumentInProcess d1 WHERE d1.docId = :docId "
			+ " AND d1.clientId =:clientId AND d1.active = TRUE AND (d1.toUser in (:userId) OR d1.delegaterId in (:userId)) "
//			+ " AND (d1.toUser != d1.frUser) "
			+ "ORDER BY d.id DESC)")
	List<DocumentInProcess> findByToUsersAndDocId2(Long docId, List<Long> userId, Long clientId);

	@Query("SELECT d FROM DocumentInProcess d "
			+ " WHERE d.docId = :docId AND d.clientId =:clientId AND d.active = TRUE "
			+ " AND (d.toUser = :userId OR d.delegaterId =:userId)"
			+ " AND d.step IN (SELECT MAX(d1.step) FROM DocumentInProcess d1 WHERE d1.docId = :docId AND d1.step <= :step"
			+ " AND d1.clientId =:clientId AND d1.active = TRUE AND (d1.toUser = :userId OR d1.delegaterId =:userId) ORDER BY d.id DESC)")
	List<DocumentInProcess> findByDocIdAndUserIdAndLessStep(Long docId, Long userId, int step, Long clientId);
	
	@Query("SELECT d FROM DocumentInProcess d "
			+ " WHERE d.docId = :docId AND d.clientId =:clientId AND d.active = TRUE AND d.step > :step"
			)
	List<DocumentInProcess> findByDocIdAndGreaterStep(Long docId, int step, Long clientId);

	@Query("SELECT d FROM DocumentInProcess d "
			+ " WHERE d.docId = :docId AND d.clientId =:clientId AND d.active = TRUE AND d.step = :step AND d.frUser=:userId "
			)
	List<DocumentInProcess> findByDocIdAndStepAndFrUser(Long docId, Integer step, Long userId, Long clientId);

	@Query("SELECT d FROM DocumentInProcess d "
			+ " WHERE d.docId = :docId AND d.clientId =:clientId AND d.active = TRUE"
			+ " AND d.step IN (SELECT MAX(d1.step) FROM DocumentInProcess d1 WHERE d1.docId = :docId "
			+ " AND d1.clientId =:clientId AND d1.active = TRUE ORDER BY d.id DESC)")
	List<DocumentInProcess> getLastStepByDocId(Long docId, Long clientId);

	@Query("SELECT d FROM DocumentInProcess d "
			+ " WHERE d.docId IN (:docIds) AND d.clientId =:clientId AND d.active = TRUE AND d.handleType ='MAIN' AND d.handleStatus ='DA_XU_LY'"
			+ " AND (d.step, d.docId) IN (SELECT MAX(d1.step), d1.docId FROM DocumentInProcess d1 WHERE d1.docId IN (:docIds) "
			+ " AND d1.clientId =:clientId AND d1.active = TRUE AND d1.handleType ='MAIN' AND d1.handleStatus ='DA_XU_LY' "
			+ " GROUP BY d1.docId ORDER BY d.id DESC )")
	List<DocumentInProcess> getLastStepByDocId(List<Long> docIds, Long clientId);

	@Modifying
	@Query("UPDATE DocumentInProcess d SET d.handleStatus = 'DA_XU_LY' WHERE (d.toUser = :toUserId OR d.delegaterId =:toUserId) AND d.docId = :docId AND d.clientId = :clientId AND d.active = TRUE AND d.handleStatus IN ('CHO_XU_LY', 'DANG_XU_LY')")
	void closeRemainProcessByUserId(Long docId, Long toUserId, Long clientId);
	
	@Query("SELECT p FROM DocumentInProcess p WHERE p.docId IN (:docIds) AND p.clientId = :clientId AND p.active = true")
	List<DocumentInProcess> findByRelatedAndDocId(List<Long> docIds, Long clientId);
	
	@Query("SELECT COUNT(1) FROM DocumentInProcess p WHERE p.docId = :docId AND p.clientId = :clientId AND p.active = true AND p.handleStatus NOT IN ('DA_XU_LY', 'DA_XU_LY_UQ')")
	long countNotYetUser(Long docId, Long clientId);

	@Query("SELECT d FROM DocumentInProcess d "
			+ " WHERE d.docId IN (:docIds) AND d.clientId =:clientId AND d.active = TRUE "
//			+ " AND (d.toUser != d.frUser) "
			+ " AND (d.toUser = :userId OR d.delegaterId =:userId) "
			+ " AND d.step IN (SELECT MAX(d1.step) FROM DocumentInProcess d1 WHERE d1.docId IN (:docIds) "
			+ " AND d1.clientId =:clientId AND d1.active = TRUE AND (d1.toUser = :userId OR d1.delegaterId =:userId) "
//			+ " AND (d1.toUser != d1.frUser) "
			+ "ORDER BY d.id DESC)")
	List<DocumentInProcess> findByToUserAndDocId2(Long userId, List<Long> docIds, Long clientId);

	@Query("SELECT p FROM DocumentInProcess p WHERE p.docId =:docId AND p.clientId = :clientId "
			+ " AND p.active = TRUE AND p.toUsers.org = :orgTransferId AND p.handleStatus = 'CHUYEN_DON_VI'")
	List<DocumentInProcess> getTransferParentDoc(Long docId, Long orgTransferId, Long clientId);

	@Query("SELECT p FROM DocumentInProcess p WHERE p.docId =:docId AND p.clientId = :clientId "
			+ " AND p.active = TRUE AND p.step = :step ")
	List<DocumentInProcess> getDocumentInProcessByDocIdAndStatusAndStep(Long docId, Integer step, Long clientId);

	@Query("select p from DocumentInProcess p where p.docId = :docId and p.step = :step and p.handleStatus in (:handleStatus) and p.frUser = :frUser and p.clientId = :clientId and p.toUser <> :userId")
    List<DocumentInProcess> findHandlesInSameStepExclusive(Long frUser, Long userId, Long docId, int step, List<DocumentInHandleStatusEnum> handleStatus, Long clientId);
}
