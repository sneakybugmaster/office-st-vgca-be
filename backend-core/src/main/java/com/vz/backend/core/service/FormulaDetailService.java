package com.vz.backend.core.service;

import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.FormulaDetail;
import com.vz.backend.core.repository.IFormulaDetailRepository;
import com.vz.backend.core.repository.IRepository;

@Service
public class FormulaDetailService extends BaseService<FormulaDetail>{
	
	@Autowired
	IFormulaDetailRepository fdRepository;
	
	@Override
	public IRepository<FormulaDetail> getRepository() {
		return fdRepository;
	}
	
	public List<FormulaDetail> save(Long fId, List<FormulaDetail> list) {
		if (BussinessCommon.isEmptyList(list))
			return Collections.emptyList();

		list.forEach(i -> i.setFId(fId));

		return fdRepository.saveAll(list);
	}
}
