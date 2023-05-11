package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.TaskRelated;
import com.vz.backend.core.repository.IRepository;
@Repository
public interface ITaskRelatedRepository extends IRepository<TaskRelated>{

	List<TaskRelated> findByTaskIdAndClientIdAndActiveTrue(Long taskId, Long clientId);

}
