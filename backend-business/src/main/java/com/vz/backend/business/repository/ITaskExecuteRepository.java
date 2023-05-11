package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.TaskExecute;
import com.vz.backend.business.dto.TaskExecuteByNodeDto;
import com.vz.backend.core.repository.IRepository;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Repository
public interface ITaskExecuteRepository extends IRepository<TaskExecute> {

	/**
	 * Hiển thị tất cả người dùng trong công việc
	 *
	 * @param taskId
	 * @return
	 */
	@Query("SELECT t FROM TaskExecute t WHERE t.clientId = :clientId AND t.active = TRUE AND t.taskId = :taskId "
			+ " ORDER BY (CASE WHEN t.isExcute is null THEN 3 WHEN t.isExcute is FALSE THEN 2 ELSE 1 END) ASC")
	List<TaskExecute> findByTaskId(Long taskId, Long clientId);

	@Query(value = "SELECT t FROM TaskExecute t WHERE t.userId = :userId and t.isCombination =:isCombination ORDER BY t.important DESC")
	List<TaskExecute> findByUserIdAndCombination(Long userId, Boolean isCombination);

	@Query(value = "SELECT t FROM TaskExecute t WHERE t.userId = :userId and t.isExcute =:isExcute and t.taskId =:taskId")
	List<TaskExecute> findBytaskEx(@Param("userId") Long userId, @Param(value = "isExcute") Boolean isExcute,
			@Param(value = "taskId") Long taskId);

	@Query(value = "SELECT t FROM TaskExecute t WHERE t.userId = :userId and t.status =:status and t.task.active is TRUE")
	List<TaskExecute> statusTask(@Param("userId") Long userId, @Param("status") Integer status);

	TaskExecute findByTaskIdAndUserIdAndClientIdAndActive(Long taskId, Long id, Long clientId, boolean active);

	@Query(value = "SELECT count(1) > 0 FROM TaskExecute t WHERE t.taskId = :taskId and (t.userId = :userId or t.createBy =:userId) and t.task.active is TRUE and t.active = TRUE and t.clientId =:clientId")
	boolean userInTask(Long taskId, Long userId, Long clientId);

	List<TaskExecute> findByTaskIdAndClientIdAndActiveTrue(Long taskId, Long clientId);

	@Query("SELECT tx FROM TaskExecute tx WHERE tx.taskId =:taskId AND tx.clientId = :clientId AND tx.active = TRUE AND tx.userId =:userId "
			+ " AND tx.step IN (SELECT MAX(tx1.step) FROM TaskExecute tx1 WHERE tx1.taskId =:taskId AND tx1.clientId = :clientId AND tx1.active = TRUE AND tx1.userId =:userId)")
	List<TaskExecute> findByTaskIdAndUserId(Long taskId, Long userId, Long clientId);

	List<TaskExecute> findByTaskIdAndStepAndClientId(Long taskId, int step, Long clientId);

	@Query("SELECT tx FROM TaskExecute tx WHERE tx.taskId =:taskId AND tx.clientId = :clientId AND tx.active = TRUE "
			+ " AND tx.step IN (SELECT MAX(tx1.step) FROM TaskExecute tx1 WHERE tx1.taskId =:taskId AND tx1.clientId = :clientId AND tx1.active = TRUE)")
	List<TaskExecute> getLastStepByTaskId(Long taskId, Long clientId);

	@Query("SELECT tx FROM TaskExecute tx WHERE tx.taskId =:taskId AND tx.clientId = :clientId AND tx.active = TRUE ")
	List<TaskExecute> getByTaskIdAndClientId(Long taskId, Long clientId);

	@Query("SELECT NEW com.vz.backend.business.dto.TaskExecuteByNodeDto(tx.userId, tx.orgId, tx.groupId, tx.node, tx.type) "
			+ " FROM TaskExecute tx "
			+ " WHERE tx.clientId=:clientId AND (tx.node=:nodeId) AND tx.active IS TRUE AND tx.taskId =:taskId "
			)
	List<TaskExecuteByNodeDto> getUserByNodeId(Long taskId, Long nodeId, Long clientId);

	@Query("SELECT tx FROM TaskExecute tx WHERE tx.isExcute is false and tx.clientId=:clientId and tx.taskId=:taskId and tx.active is true")
	List<TaskExecute> sizetaskPh(Long taskId, Long clientId);

	@Query("SELECT tx FROM TaskExecute tx WHERE tx.taskId =:taskId AND tx.clientId = :clientId AND tx.active = TRUE" +
			" and tx.isCombination = true and tx.step = ( select max(a.step) from TaskExecute a where a.active = true " +
			"and a.taskId=:taskId and a.clientId=:clientId )and tx.userId = :userId")
	TaskExecute findBymaxStepAndtaskId(Long taskId, Long clientId,long userId);
}
