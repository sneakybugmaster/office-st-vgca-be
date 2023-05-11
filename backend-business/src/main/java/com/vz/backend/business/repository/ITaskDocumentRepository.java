package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.TaskDocument;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ITaskDocumentRepository extends IRepository<TaskDocument> {

	List<TaskDocument> findByTaskIdAndActiveTrueAndClientId(Long taskId, Long clientId);
}
