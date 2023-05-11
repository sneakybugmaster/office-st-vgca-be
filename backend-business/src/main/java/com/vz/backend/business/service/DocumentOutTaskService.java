package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.DocumentOutTask;
import com.vz.backend.business.repository.IDocumentOutTaskRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class DocumentOutTaskService extends BaseService<DocumentOutTask>{
	@Autowired
	IDocumentOutTaskRepository docOutTask;

	@Override
	public IRepository<DocumentOutTask> getRepository() {
		return docOutTask;
	}

	public void add(Long docOutId, Set<Long> taskIds) {
		Set<Long> listTaskIds = docOutTask.findTaskIdByDocOutIdAndClientId(docOutId, BussinessCommon.getClientId());
		Set<Long> deleteIds = new HashSet<>(listTaskIds);
		deleteIds.removeAll(taskIds);
		Set<Long> addIds = new HashSet<>(taskIds);
		addIds.removeAll(listTaskIds);
		if (!deleteIds.isEmpty()) docOutTask.deleteByDocOutIdAndTaskIdIn(docOutId, deleteIds);
		if (!addIds.isEmpty()) {
			List<DocumentOutTask> listAdd = new ArrayList<>();
			for (Long id : addIds) {
				listAdd.add(new DocumentOutTask(docOutId, id));
			}
			docOutTask.saveAll(listAdd);
		}
	}
}
