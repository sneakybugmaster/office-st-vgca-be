package com.vz.backend.business.service;

import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.AssignTask;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class AssignTaskService extends BaseService<AssignTask> {

	@Override
	public IRepository<AssignTask> getRepository() {
		// TODO Auto-generated method stub
		return null;
	}

}
