package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.TaskHistory;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ITaskHistoryRepository extends IRepository<TaskHistory> {

	List<TaskHistory> findByTaskIdAndClientIdAndActiveTrue(Long taskId, Long clientId);

}
