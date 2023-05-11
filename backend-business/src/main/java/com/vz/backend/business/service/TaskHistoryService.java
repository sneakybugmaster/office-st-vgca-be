package com.vz.backend.business.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.Task;
import com.vz.backend.business.domain.TaskHistory;
import com.vz.backend.business.repository.ITaskHistoryRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class TaskHistoryService extends BaseService<TaskHistory> {
	@Autowired
	private ITaskHistoryRepository taskHistoryRepository;

	@Override
	public IRepository<TaskHistory> getRepository() {
		return taskHistoryRepository;
	}

	@Override
	public TaskHistory save(TaskHistory taskHistory) {
		taskHistory.valids();
		return taskHistoryRepository.save(taskHistory);
	}

	public TaskHistory save(Task task) {
		return taskHistoryRepository.save(new TaskHistory(task));
	}

	public List<TaskHistory> findByTaskId(Long taskId) {
		return taskHistoryRepository.findByTaskIdAndClientIdAndActiveTrue(taskId, BussinessCommon.getClientId());
	}
}
