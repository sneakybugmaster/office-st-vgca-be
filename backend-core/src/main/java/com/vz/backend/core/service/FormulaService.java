package com.vz.backend.core.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Formula;
import com.vz.backend.core.domain.FormulaDetail;
import com.vz.backend.core.dto.FormulaDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IFormulaRepository;
import com.vz.backend.core.repository.IRepository;

@Service
public class FormulaService extends BaseService<Formula>{

	@Autowired
	IFormulaRepository fRepository;
	
	@Override
	public IRepository<Formula> getRepository() {
		return fRepository;
	}
	
	@Autowired
	FormulaDetailService fdService;
	
	@Override
	public Formula add(Formula f) {
		f.valids();
		List<FormulaDetail> details = f.getDetails();
		if (f.getId() != null) {
			Formula old = valid(f.getId(), Message.NOT_FOUND_FORMULA);
			f = old.set(f);
		}
		fRepository.save(f);
		try {
			fdService.save(f.getId(), details);
			return f;
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.ERROR_SYS);
		}
	}
	
	public Page<Formula> list(FormulaDto dto) {
		return fRepository.list(dto, BussinessCommon.getClientId(), BussinessCommon.toPage(dto));
	}

	public List<FormulaDto> list() {
		return fRepository.list(BussinessCommon.getClientId());
	}
}
