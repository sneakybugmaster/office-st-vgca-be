package com.vz.backend.business.repository;

import java.util.List;
import java.util.Set;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentOutTask;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentOutTaskRepository extends IRepository<DocumentOutTask>{

	List<DocumentOutTask> findByDocOutIdAndClientId(Long docOutId, Long clientId);
	
	@Query("SELECT DISTINCT d.taskId FROM DocumentOutTask d WHERE d.docOutId = :docOutId AND d.clientId = :clientId")
	Set<Long> findTaskIdByDocOutIdAndClientId(Long docOutId, Long clientId);

	@Modifying
	void deleteByDocOutIdAndTaskIdIn(Long docOutId, Set<Long> deleteIds);

}
