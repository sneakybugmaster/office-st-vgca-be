package com.vz.backend.business.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.Task;
import com.vz.backend.business.domain.TaskExecute;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.business.dto.ResultQuickSearchDto;
import com.vz.backend.business.dto.SearchTask;
import com.vz.backend.business.dto.TaskDto;
import com.vz.backend.business.dto.TaskExecuteDto;
import com.vz.backend.business.dto.fullreport.SimpleProcessTask;
import com.vz.backend.business.dto.kpi.KPIDataDto;
import com.vz.backend.business.dto.task.TaskListDto;
import com.vz.backend.core.repository.IRepository;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Repository
public interface ITaskRepository extends IRepository<Task> {

	@Query(value = "SELECT t FROM Task t WHERE t.active = :active and t.userAssignId = :userId and t.active=true and t.clientId=:clientId and t.status=:status ORDER BY t.important DESC")
	List<Task> findByAssignByAndStatus(@Param(value = "active") Boolean active, @Param(value = "userId") Long assignBy,
			@Param(value = "clientId") Long clientId, @Param(value = "status") Integer status);

	/**
	 * Danh sách công việc cá nhân
	 * @param assignedWork 
	 *
	 * @param userId
	 * @param status
	 * @return
	 */
	@Query(value = "SELECT DISTINCT t FROM Task t JOIN TaskExecute tx on (tx.taskId = t.id) WHERE t.active = :active AND tx.active = :active AND t.status IN (:status) "
			+ " AND (:userAssignId IS NULL OR t.userAssignId =:userAssignId) "
			+ " AND (:userId IS NULL OR tx.userId =:userId) and " +
			" ((:listId) is null or t.id NOT IN (:listId)) order by t.updateDate desc "
			)
	Page<Task> findByUserExecute(@Param(value = "active") Boolean active,
//			@Param(value = "assignedWork") Boolean assignedWork, 
			@Param(value = "userId") Long userId,
			@Param(value = "userAssignId") Long userAssignId,
			@Param(value = "status") List<Integer> status,@Param(value = "listId") List<Long> listId, Pageable pageable);

	@Query(value = "SELECT DISTINCT t FROM Task t " +
			" JOIN TaskExecute tx on (tx.taskId = t.id) WHERE t.active = :active AND tx.active = :active AND t.status IN (:status) " +
			" AND (t.parentId is null or t.parentId=:parentId)"
			+ " AND (:userAssignId IS NULL OR t.userAssignId =:userAssignId) and " +
			" (:userId IS NULL OR tx.userId =:userId) and " +
			" ((:listId) is null or t.id NOT IN (:listId)) order by t.updateDate desc"
	)
	Page<Task> findTaskByUserExecuteAndParentNull(@Param(value = "active") Boolean active,
								 @Param(value = "userAssignId") Long userAssignId, @Param(value = "userId") Long userId,
								 @Param(value = "status") List<Integer> status,@Param(value = "listId") List<Long> listId,@Param(value = "parentId") Long parentId, Pageable pageable);

	@Query(value = "SELECT new com.vz.backend.business.dto.TaskDto(t.id, t.taskName, t.userAssign.fullName) FROM Task t WHERE t.active = :active and t.userExcutePrimaryId = :userId and t.status IN (:status) ORDER BY t.createDate DESC")
	Page<TaskDto> findTaskByUserExecute(boolean active, Long userId, List<Integer> status, Pageable castToPageable);

	/**
	 * Danh sách công việc đã giao
	 * @param compareDay 
	 *
	 * @param userId
	 * @param status
	 * @return
	 */
	@Query(value = "SELECT NEW com.vz.backend.business.dto.task.TaskListDto(t, t.status, tx.node, tx.reviewRequired, t.important, false, tx.nextNode)  "
			+ " FROM Task t JOIN TaskExecute tx ON tx.taskId = t.id AND t.active = tx.active AND t.clientId = tx.clientId "
			+ " WHERE t.active = :active AND t.clientId = :clientId"
			+ " AND t.userAssignId = :userId AND t.status IN (:status) AND tx.userId =:userId AND tx.step = 1"
			+ " AND (:dayLeft IS NULL"
			+ "		OR :dayLeft = "
			+ "				CASE WHEN (COALESCE(t.endDate, NULL) IS NOT NULL AND (t.endDate < current_date())) THEN -1 "
			+ "					 WHEN (COALESCE(t.endDate, NULL) IS NULL OR t.endDate > current_date() + 2)  THEN 3 "
			+ "					 ELSE 0 END)"
			+ " GROUP BY t, t.status, tx.node, tx.reviewRequired, t.important, tx.nextNode"
			)
	//ORDER BY (CASE WHEN t.important is null THEN 3 WHEN t.important is FALSE THEN 2 ELSE 1 END) ASC, t.createDate DESC
	Page<TaskListDto> findByUserAssign(Integer dayLeft, Boolean active, Long userId, List<Integer> status,
			Long clientId, Pageable pageable);

	/**
	 * Danh sách công việc hỗ trợ
	 * @param compareDay 
	 *
	 * @param userId
	 * @param status
	 * @return
	 */
	@Query(value = "SELECT new com.vz.backend.business.dto.TaskExecuteDto(tx.id, tx.comment, t.id, t.taskName, t.startDate, t.endDate, tx.progress, tx.important, t.codeTask, t.userAssign.fullName, tx.status, t.approveStatus, t.status, tx.read) "
			+ " FROM Task t inner join TaskExecute tx on t.id = tx.taskId and tx.active = t.active and tx.clientId = t.clientId"
			+ " WHERE t.active = :active AND t.status IN (:status) "
			+ " AND (:dayLeft IS NULL"
			+ "		OR :dayLeft = "
			+ "				CASE WHEN (COALESCE(t.endDate, NULL) IS NOT NULL AND (t.endDate < current_date())) THEN -1 "
			+ "					 WHEN (COALESCE(t.endDate, NULL) IS NULL OR t.endDate > current_date() + 2)  THEN 3 "
			+ "					 ELSE 0 END)"
			+ " AND tx.userId = :userId AND tx.isCombination =:isCombination" + " AND t.clientId = :clientId "
//			+ " ORDER BY "
//			+ " (CASE WHEN tx.important is null THEN 3 WHEN tx.important is FALSE THEN 2 ELSE 1 END) ASC,"
//			+ " t.startDate DESC"
			)
	Page<TaskExecuteDto> findByIdAndStatus(Integer dayLeft, Boolean active, List<Integer> status, Boolean isCombination, Long userId,
			Long clientId, Pageable pageable);


	@Query(value = "SELECT t  "
			+ " FROM Task t JOIN TaskExecute tx ON tx.taskId = t.id AND t.active = tx.active AND t.clientId = tx.clientId "
			+ " WHERE t.active = :active AND t.clientId = :clientId"
			+ " AND t.userAssignId = :userId AND t.status =:status AND tx.userId =:userId AND tx.step = 1"
	)
		//ORDER BY (CASE WHEN t.important is null THEN 3 WHEN t.important is FALSE THEN 2 ELSE 1 END) ASC, t.createDate DESC
	List<Task> findByListUserAssign(Boolean active, Long userId, Integer status,
									   Long clientId);

	@Query(value = "SELECT t FROM Task t WHERE  t.active = :active and t.userAssignId =:userId and t.status = :status")
	List<Task> findByStatus(@Param(value = "active") Boolean active, @Param(value = "userId") Long UserId,
			@Param(value = "status") Integer status);
	
	public static final String FIND_ALL_SQL = "SELECT NEW com.vz.backend.business.dto.task.TaskListDto(t, '') "
		+ " FROM Task t "
		+ " JOIN TaskExecute tx ON tx.taskId = t.id AND t.active = tx.active AND t.clientId = tx.clientId "
		+ " LEFT JOIN ObjectTag ot ON ot.objId = t.id AND ot.active = TRUE AND ot.type = 'GIAO_VIEC' "
		+ " LEFT JOIN Tag tg ON tg.id = ot.tagId AND tg.active = TRUE AND tg.createBy = :#{#dto.userId} "
		+ " WHERE t.active = TRUE AND t.clientId = :#{#dto.clientId}"
		+ " AND (:#{#dto.userAssignId} IS NULL OR t.userAssignId = :#{#dto.userAssignId})"
		+ " AND (:#{#dto.dayLeft} IS NULL"
		+ "		OR :#{#dto.dayLeft} = "
		+ "				CASE WHEN (COALESCE(t.endDate, NULL) IS NOT NULL AND (t.endDate < current_date())) THEN -1 "
		+ "					 WHEN (COALESCE(t.endDate, NULL) IS NULL OR t.endDate > current_date() + 2)  THEN 3 "
		+ "					 ELSE 0 END)"
//		+ " AND ((:#{#dto.userCombinationId} IS NULL OR tx.userId = :#{#dto.userCombinationId}) "
//		+ " 	OR (:#{#dto.userExcutePrimaryId} IS NULL OR t.userExcutePrimaryId = :#{#dto.userExcutePrimaryId}))"
		+ " AND (:#{#dto.userExcutePrimaryName} IS NULL OR t.id IN (SELECT tx2.taskId FROM TaskExecute tx2 WHERE (tx2.step = ( select MAX(txStep.step) FROM TaskExecute txStep WHERE txStep.taskId=tx2.taskId and txStep.active = true and txStep.isExcute=true)) and tx2.isExcute=true and LOWER(tx2.user.fullName) LIKE %:#{#dto.userExcutePrimaryName}%  AND (:#{#dto.userStatus} is null or (  tx2.isCombination is :#{#dto.userStatus} and tx2.isExcute is not :#{#dto.userStatus} )) AND tx2.clientId = :#{#dto.clientId} AND tx2.active = TRUE GROUP BY tx2.taskId))"
		+ " AND (:#{#dto.orgId} IS NULL OR t.id IN (SELECT tx3.taskId FROM TaskExecute tx3 WHERE tx3.clientId = :#{#dto.clientId} AND tx3.active = TRUE AND (tx3.orgId = :#{#dto.orgId} OR tx3.user.org = :#{#dto.orgId}) GROUP BY tx3.taskId))" 
		+ " AND t.status IN (:#{#dto.status}) "
		+ " AND (:#{#dto.taskName} IS NULL OR LOWER(t.taskName) LIKE %:#{#dto.taskName}% OR LOWER(tg.name) LIKE %:#{#dto.taskName}%)"
		+ " AND (:#{#dto.taskFieldId} IS NULL OR t.taskFieldId = :#{#dto.taskFieldId})"
		+ " AND (:#{#dto.codeTask} IS NULL OR LOWER(t.codeTask) LIKE %:#{#dto.codeTask}% OR LOWER(tg.name) LIKE %:#{#dto.codeTask}%)"
		+ " AND (COALESCE(:#{#dto.startDate}, NULL) IS NULL OR t.startDate > :#{#dto.startDate})"
		+ " AND (COALESCE(:#{#dto.endDate}, NULL) IS NULL OR t.endDate < :#{#dto.endDate})"
		+ " AND (:#{#dto.priorityId} IS NULL OR t.priorityId = :#{#dto.priorityId}) "
		+ " AND (tx.taskId, tx.step, tx.updateDate) IN (SELECT tx1.taskId, MAX(tx1.step), MAX(tx1.updateDate) FROM TaskExecute tx1 WHERE tx1.clientId = :#{#dto.clientId} AND tx1.active = TRUE "
		+ " 	AND tx1.status IN (:#{#dto.status}) "
		+ " 	AND (tx1.userId = :#{#dto.userId})"
		+ "		GROUP BY tx1.taskId"
		+ " )"
		+ " GROUP BY t.id, t.important, t.startDate, t.userExcute.fullName"
		+ " ORDER BY t.important ASC, t.startDate DESC, t.userExcute.fullName DESC "
		;

	@Query(value = FIND_ALL_SQL )
	Page<TaskListDto> findByMultiCondition(SearchTask dto, Pageable castToPageable);
	
	@Query(value = FIND_ALL_SQL)
	List<TaskListDto> findByMultiCondition(SearchTask dto);

	@Query(value = "SELECT t FROM Task t WHERE t.active =:active and t.status IN (:status) and t.orgId in :orgId"
			+ " AND (:dayLeft IS NULL"
			+ "		OR :dayLeft = "
			+ "				CASE WHEN (COALESCE(t.endDate, NULL) IS NOT NULL AND (t.endDate < current_date())) THEN -1 "
			+ "					 WHEN (COALESCE(t.endDate, NULL) IS NULL OR t.endDate > current_date() + 2)  THEN 3 "
			+ "					 ELSE 0 END)"
			)
	Page<Task> getListTaskLead(@Param(value = "dayLeft") Integer dayLeft, @Param(value = "active") Boolean active, @Param(value = "status") List<Integer> status,
			@Param(value = "orgId") List<Long> orgId, Pageable pageable);

	@Query(value = "SELECT new com.vz.backend.business.dto.TaskDto(t.id, t.taskName, t.userAssign.fullName) FROM Task t WHERE t.id IN (:ids)")
	List<TaskDto> findTaskByIds(List<Long> ids);

	@Query("SELECT new com.vz.backend.business.dto.fullreport.SimpleProcessTask(t.id, t.updateDate, tx.status, t.endDate) FROM Task t "
			+ "INNER JOIN TaskExecute tx on t.id = tx.taskId and tx.active = t.active and tx.clientId = t.clientId "
			+ "WHERE t.active is TRUE AND tx.userId = :userId AND t.clientId = :clientId AND "
			+ "(coalesce(:startDate, null) is NULL or t.updateDate BETWEEN :startDate and :endDate)")
	List<SimpleProcessTask> fullReport(Long userId, Long clientId, Date startDate, Date endDate);

	@Query("SELECT new com.vz.backend.business.dto.TaskDto(t.task.id, t.task.taskName, t.task.userAssign.fullName) FROM TaskDocument t inner join Task ta"
			+ " on t.taskId = ta.id WHERE 1=1 AND t.active = TRUE and ta.active is true AND t.docId = :docId AND t.typeDocument = :docType")
	List<TaskDto> getListTaskDtoByDocIdAndDocType(Long docId, boolean docType);
	
	@Query("SELECT t.task FROM TaskDocument t "
			+ "WHERE 1=1 AND t.active = TRUE AND t.docId = :docId AND t.typeDocument = :docType")
	List<Task> getListTaskByDocIdAndDocType(Long docId, boolean docType);
	
	@Query("SELECT count(t.id) FROM Task t "
			+ "INNER JOIN TaskExecute tx on t.id = tx.taskId and tx.active = t.active and tx.clientId = t.clientId "
			+ "WHERE t.active is TRUE AND tx.userId = :userId AND t.clientId = :clientId AND t.endDate < current_date()")
	Integer countOverDue(Long userId, Long clientId);

	@Query("SELECT count(*) > 0 FROM Task t INNER JOIN TaskExecute te ON t.id = te.taskId "
			+ "WHERE t.id = :taskId AND (t.userAssignId = :currUser OR te.userId = :currUser)")
	boolean checkPermission(Long taskId, Long currUser);

	@Query("SELECT new com.vz.backend.business.dto.ReportDocByTypeDto(tx.taskId, tx.userId, t.userAssignId, tx.isCombination, tx.step, t.status) "
			+ "FROM TaskExecute tx JOIN Task t ON tx.taskId = t.id "
			+ "WHERE tx.clientId =:clientId AND t.clientId =:clientId AND tx.active IS TRUE AND t.active IS TRUE "
			+ "AND (tx.status < 4) AND (tx.userId = :userId OR t.userAssignId = :userId ) AND t.status < 4 "
			+ "GROUP BY tx.taskId, tx.userId, t.userAssignId, tx.isCombination, tx.step, t.status ")
	List<ReportDocByTypeDto> reportDocByType(Long userId, Long clientId);

	List<Task> findByParentIdAndClientIdAndActiveTrue(Long id, Long clientId);

	List<Task> findByClientIdAndOrgIdInAndActiveTrue(Long clientId, List<Long> orgIds);
	
	@Query("SELECT new com.vz.backend.business.dto.kpi.KPIDataDto(p) FROM TaskExecute p "
			+ " WHERE p.userId IN (:userIds) AND p.clientId =:clientId AND p.active = TRUE AND p.task.active = TRUE"
			+ " AND (COALESCE(p.updateDate, p.createDate) IS NULL OR p.updateDate BETWEEN :startDate AND :endDate)")
	List<KPIDataDto> findAllByToUser(List<Long> userIds, Long clientId, Date startDate, Date endDate);

	@Query("SELECT new com.vz.backend.business.dto.kpi.KPIDataDto(tx) FROM Task t "
			+ " LEFT JOIN TaskExecute tx ON tx.taskId = t.id "
			+ " WHERE tx.clientId =:clientId AND t.clientId =:clientId AND tx.active IS TRUE AND t.active IS TRUE "
			+ " AND (:userId IS NULL OR t.userAssignId = :userId OR tx.userId = :userId) "
			+ " AND (COALESCE(:startDate, NULL) IS NULL OR COALESCE(:endDate, NULL) IS NULL OR COALESCE(tx.updateDate, tx.createDate) IS NULL OR tx.updateDate BETWEEN :startDate AND :endDate)")
	List<KPIDataDto> findAllByToUser(Long userId, Long clientId, Date startDate, Date endDate);
	
	@Query(name = "Common.union", nativeQuery = true) 
	List<ResultQuickSearchDto> quickSearchAllObject(Date date, boolean clericalOrg, Long userId, boolean isLibrarianDocIn, List<Long> userIds,
			String text, boolean isLibrarianDocOut, Long orgId, boolean lead, Long orgTopLv, List<Long> orgIdTopLvs, List<Long> orgIdUnitLvs, Long clientId);

	@Query(value = "SELECT new com.vz.backend.business.dto.TaskDto(t.id, t.taskName, t.userAssign.fullName) FROM Task t "
			+ " WHERE t.active = TRUE AND t.clientId=:clientId AND t.orgId IN (:orgIds) AND t.parentId IS NULL ORDER BY t.createDate DESC")
	Page<TaskDto> getSubTasks(List<Long> orgIds, Long clientId, Pageable pageable);

	@Query(value = "SELECT NEW com.vz.backend.business.dto.task.TaskListDto(t, tx.status, tx.node, tx.reviewRequired, tx.important, tx.close, tx.nextNode,tx.progress) "
			+ " FROM Task t JOIN TaskExecute tx ON tx.taskId = t.id AND t.active = tx.active AND t.clientId = tx.clientId "
			+ " WHERE t.active = TRUE AND t.clientId = :clientId"
			+ " AND (:dayLeft IS NULL"
			+ "		OR :dayLeft = "
			+ "				CASE WHEN (COALESCE(t.endDate, NULL) IS NOT NULL AND (t.endDate < current_date())) THEN -1 "
			+ "					 WHEN (COALESCE(t.endDate, NULL) IS NULL OR t.endDate > current_date() + 2)  THEN 3 "
			+ "					 ELSE 0 END)"
			+ " AND tx.status IN (:statusList) "
			+ " AND tx.userId = :userId AND tx.step > 1"
			+ " AND ((:isExcute IS TRUE AND tx.isExcute = TRUE) OR (:isExcute IS FALSE AND tx.isCombination IS TRUE))"
			+ " AND (tx.taskId, tx.step) IN (SELECT tx1.taskId, MAX(tx1.step) FROM TaskExecute tx1 WHERE tx1.clientId = :clientId AND tx1.active = TRUE "
			+ " 	AND tx1.status IN (:statusList) "
			+ " 	AND tx1.userId = :userId AND tx1.step > 1"
			+ " 	AND ((:isExcute IS TRUE AND tx1.isExcute = TRUE) OR (:isExcute IS FALSE AND tx1.isCombination IS TRUE))"
			+ "		GROUP BY tx1.taskId"
			+ " )"
//			+ " ORDER BY t.important ASC, t.startDate DESC"
	)
	Page<TaskListDto> listByTypeAndStatus(Long userId, Boolean isExcute, List<Integer> statusList, Integer dayLeft,
										  Long clientId, Pageable pageable);

	@Query("SELECT DISTINCT(t.task) FROM TaskExecute t WHERE t.step IS NULL AND t.clientId = :clientId AND t.active = TRUE ")
	List<Task> getTaskNotRecordFirstStepYet(Long clientId);

	@Query("SELECT t FROM TaskExecute t WHERE t.step IS NULL AND t.clientId = :clientId AND t.active = TRUE ")
	List<TaskExecute> getTaskExeIsNull(Long clientId);

	@Query("SELECT NEW com.vz.backend.business.dto.task.TaskListDto(tx.taskId, tx.user.fullName, tx.user.orgModel.name) FROM TaskExecute tx WHERE tx.clientId = :clientId AND tx.active = TRUE AND tx.taskId IN (:taskIds) AND tx.isExcute=true "
			+ "AND (tx.taskId, tx.step) IN (SELECT tx1.taskId, MAX(tx1.step) FROM TaskExecute tx1 WHERE tx1.clientId = :clientId AND tx1.active = TRUE AND tx1.taskId IN (:taskIds) GROUP BY tx1.taskId) "
			+ "GROUP BY tx.taskId, tx.user.fullName, tx.user.orgModel.name")
	List<TaskListDto> getUserByMaxStepAndTaskIds(List<Long> taskIds, Long clientId);

	@Query("SELECT t FROM Task t WHERE  t.clientId = :clientId AND t.active = TRUE and t.parentId=:parentId ")
	List<Task> findByParentIdAndActive(Long clientId,Long parentId);

}