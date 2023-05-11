package com.vz.backend.core.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.DelegateFlow;
import com.vz.backend.core.dto.DelegateFlowDto;
import com.vz.backend.core.repository.IDelegateFlowRepository;
import com.vz.backend.core.repository.IRepository;

@Service
public class DelegateFlowService {

	@Autowired
	IDelegateFlowRepository delegateFlowRepo;

	public IRepository<DelegateFlow> getRepository() {
		return delegateFlowRepo;
	}

	public DelegateFlow save(Long from, Long to) {
		if (from == null || to == null || from.equals(to)) {
			throw new RestClientException(Message.ACTION_FAILED);
		}
		DelegateFlow df = new DelegateFlow();
		df.setFromPositionId(from);
		df.setToPositionId(to);
		try {
			delegateFlowRepo.save(df);
		} catch (Exception e) {
			throw new RestClientException(Message.ACTION_FAILED);
		}
		return df;
	}

	public Page<DelegateFlowDto> list(Pageable pageable) {
		return delegateFlowRepo.list(BussinessCommon.getClientId(), pageable);
	}

	public void delete(Long id) {
		delegateFlowRepo.deleteById(id);
	}

	public List<DelegateFlow> list() {
		return delegateFlowRepo.list(BussinessCommon.getClientId());
	}

	public List<Long> listFromPosition() {
		return delegateFlowRepo.listFromPosition(BussinessCommon.getClientId());
	}

}
