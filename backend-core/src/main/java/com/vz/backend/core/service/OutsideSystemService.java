package com.vz.backend.core.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.OutsideSystem;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IOutsideSystemRepository;
import com.vz.backend.core.repository.IRepository;

@Service
public class OutsideSystemService extends BaseService<OutsideSystem> {

	@Autowired
	IOutsideSystemRepository outsideSystemRepository;

	@Override
	public IRepository<OutsideSystem> getRepository() {
		return outsideSystemRepository;
	}

	public Page<OutsideSystem> findAll(Pageable page) {
		return outsideSystemRepository.findAll(page);
	}

	@Override
	public OutsideSystem add(OutsideSystem data) {
		data.valids();
		checkExistSysName(data.getName());
		return outsideSystemRepository.save(data);
	}

	public OutsideSystem update(Long id, OutsideSystem data) {
		data.valids();
		OutsideSystem old = valid(id, Message.NOT_FOUND_CONNECT_SYSTEM);
		if (!data.getName().equals(old.getName())) {
			checkExistSysName(data.getName());
		}

		old.set(data);
		return outsideSystemRepository.save(old);
	}

	private void checkExistSysName(String name) {
		OutsideSystem tmp = outsideSystemRepository.findFirstByNameAndActiveTrue(name);
		if (tmp != null) {
			throw new RestExceptionHandler(Message.EXIST_CONNECT_SYSTEM);
		}
	}

	public OutsideSystem findByDomainAndKey(String domain, String key) {
		return outsideSystemRepository.findByDomainAndKeyAndActiveTrue(domain, key);
	}

	/**
	 * Lock and unlock 
	 * @param id
	 * @param active
	 * @return
	 */
	public Boolean control(Long id, Boolean active) {
		OutsideSystem tmp = valid(id, Message.NOT_FOUND_CONNECT_SYSTEM);
		if(active == null) { //Delete control
			outsideSystemRepository.deleteById(id);
			return true;
		}
		tmp.setActive(active);
		outsideSystemRepository.save(tmp);
		return true;
	}
}
