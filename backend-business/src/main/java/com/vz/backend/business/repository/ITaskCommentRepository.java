package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.TaskComment;
import com.vz.backend.core.repository.IRepository;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Repository
public interface ITaskCommentRepository extends IRepository<TaskComment> {

	@Query(value = "SELECT t FROM TaskComment t WHERE t.taskId = :taskId and t.active=:active and t.clientId=:clientId ORDER BY t.createDate DESC")
	List<TaskComment> findByTaskIdAndActive(Long taskId, Boolean active, Long clientId);
}
