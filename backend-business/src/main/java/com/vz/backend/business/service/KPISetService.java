package com.vz.backend.business.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestClientException;

import com.vz.backend.business.domain.KPISet;
import com.vz.backend.business.domain.Targets;
import com.vz.backend.business.dto.kpi.KPISetDto;
import com.vz.backend.business.repository.IKPISetRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class KPISetService extends BaseService<KPISet> {

	@Autowired
	IKPISetRepository kpiSetRepository;
	
	@Override
	public IRepository<KPISet> getRepository() {
		return kpiSetRepository;
	}
	
	@Autowired
	TargetsService targetsService;
	
	@Override
	public KPISet add(KPISet f) {
		f.valids();
		List<Targets> kpis = f.getKpis();
		boolean isUpdate = f.getId() != null;
		if (isUpdate) {
			KPISet old = valid(f.getId(), Message.KPI_SET_NOT_FOUND);
			f = old.set(f);
		}
		try {
			f = kpiSetRepository.save(f);
			targetsService.save(f.getId(), kpis);
			return f;
		} catch (Exception e) {
			if (!isUpdate)
				kpiSetRepository.deleteById(f.getId());
			e.printStackTrace();
			throw new RestExceptionHandler(Message.KPI_SET_DATA_INVALID);
		}
	}

	public Page<KPISet> list(KPISetDto dto) {
		return kpiSetRepository.list(dto, BussinessCommon.getClientId(), BussinessCommon.toPage(dto));
	}
	
	public List<KPISetDto> list() {
		return kpiSetRepository.list(BussinessCommon.getClientId());
	}
	
	@Override
	public void deleteById(Long id) {
		KPISet set = valid(id, Message.KPI_SET_NOT_FOUND);
		try {
			set.setActive(false);
			kpiSetRepository.save(set);
		} catch (Exception e) {
			throw new RestClientException(Message.KPI_SET_USING);
		}
	}
}
