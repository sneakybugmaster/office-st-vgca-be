package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;

import com.vz.backend.business.domain.Targets;
import com.vz.backend.business.repository.ITargetsRepository;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class TargetsService extends BaseService<Targets>{
	
	@Autowired
	ITargetsRepository targetRepository;

	@Override
	public IRepository<Targets> getRepository() {
		return targetRepository;
	}
	
	public List<Targets> save(Long sId, List<Targets> allList) {
		List<Targets> rsList = new ArrayList<>();
		allList.forEach(i -> {
			i.setKpiSet(sId);
			i.valids();
			if (i.getId() != null) {
				Targets old = valid(i.getId(), Message.NOT_FOUND_KPI_TARGET);
				i = old.set(i);
			}
			rsList.add(i);
		});
		return targetRepository.saveAll(rsList);
	}
	
	@Override
	public void deleteById(Long id) {
		Targets target = valid(id, Message.KPI_SET_NOT_FOUND);
		try {
			target.setActive(false);
			targetRepository.save(target);
		} catch (Exception e) {
			throw new RestClientException(Message.KPI_SET_USING);
		}
	}
}
