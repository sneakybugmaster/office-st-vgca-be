package com.vz.backend.business.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.KPI;
import com.vz.backend.business.dto.kpi.KPIDto;
import com.vz.backend.business.repository.IKPIRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class KPIService extends BaseService<KPI> {

	@Autowired
	IKPIRepository kpiRepository;

	@Override
	public IRepository<KPI> getRepository() {
		return kpiRepository;
	}

	public Page<KPI> list(KPIDto dto) {
		return kpiRepository.list(dto, BussinessCommon.getClientId(), BussinessCommon.toPage(dto));
	}

	public List<KPI> list() {
		return kpiRepository.findByClientIdAndActive(BussinessCommon.getClientId(), true);
	}

	@Override
	public KPI add(KPI f) {
		f.valids();
		boolean isUpdate = f.getId() != null;
		if (isUpdate) {
			KPI old = valid(f.getId(), Message.KPI_NOT_FOUND);
			f = old.set(f);
		}
		try {
			f = kpiRepository.save(f);
			return f;
		} catch (Exception e) {
			if (isUpdate)
				kpiRepository.deleteById(f.getId());
			e.printStackTrace();
			throw new RestExceptionHandler(Message.KPI_SET_DATA_INVALID);
		}
	}
}
