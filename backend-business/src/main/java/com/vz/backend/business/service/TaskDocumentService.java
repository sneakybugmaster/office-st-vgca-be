package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManagerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.TaskDocument;
import com.vz.backend.business.repository.ITaskDocumentRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Service
public class TaskDocumentService extends BaseService<TaskDocument> {

	@Autowired
	ITaskDocumentRepository repository;

	@Autowired
	EntityManagerFactory entityManagerFactory;

	@Override
	public IRepository<TaskDocument> getRepository() {
		return repository;
	}

	/**
	 * Hiển thị tất cả người dùng trong công việc
	 *
	 * @param userId
	 * @param status
	 * @param pageable
	 * @return
	 */
	public List<TaskDocument> findByTaskId(Long taskId) {
		return repository.findByTaskIdAndActiveTrueAndClientId(taskId, BussinessCommon.getClientId());
	}
	
	public List<TaskDocument> saveTaskDocument(Long taskId, List<TaskDocument> newsList) {
		List<TaskDocument> rsList = new ArrayList<>();
		List<TaskDocument> oldList = findByTaskId(taskId);
		newsList.forEach(i-> i.setTaskId(taskId));
		
		List<TaskDocument> all = new ArrayList<>();
		all.addAll(oldList);
		all.addAll(newsList);
		
		all.forEach(a -> {
			if (!oldList.contains(a) && newsList.contains(a)) {
				// add
				rsList.add(a);
			} else if (oldList.contains(a) && !newsList.contains(a)) {
				// del
				a.setActive(false);
				rsList.add(a);
			}
		});
		
		return repository.saveAll(rsList);
	}
}
