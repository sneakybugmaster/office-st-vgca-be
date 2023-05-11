package com.vz.backend.business.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import com.vz.backend.business.dto.ContactSearchDto;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.dto.ContactDto;
import com.vz.backend.core.dto.SearchDto;
import com.vz.backend.core.repository.IUserRepository;

@Service
public class ContactService {
	
	@Autowired
	private IUserRepository userRepository;

	public Page<ContactDto> all(SearchDto dto) {
		Sort sort = Sort.by(
				Sort.Order.asc("orgModel.level"),
				Sort.Order.asc("orgModel.order"),
				Sort.Order.asc("positionModel.order"));
		Pageable pageable = PageRequest.of(dto.getPage() - 1, dto.getSize(), sort);
		return userRepository.allContact(BussinessCommon.getClientId(), pageable);
	}

	public Page<ContactDto> search(ContactSearchDto dto) {
		Sort sort = Sort.by(
				Sort.Order.asc("orgModel.level"),
				Sort.Order.asc("orgModel.order"),
				Sort.Order.asc("positionModel.order"));
		if (dto.q != null) {
			dto.q = dto.q.trim().toLowerCase();
			if (dto.q.length() == 0) {
				dto.q = null;
			}
		}

		Pageable pageable = PageRequest.of(dto.getPage() - 1, dto.getSize(), sort);
		return userRepository.searchContact(dto.q, dto.orgId, BussinessCommon.getClientId(), pageable);
	}

	public List<ContactDto> search2(ContactSearchDto dto) {
		Sort sort = Sort.by(
				Sort.Order.asc("orgModel.level"),
				Sort.Order.asc("orgModel.order"),
				Sort.Order.asc("positionModel.order"));
		if (dto.q != null) {
			dto.q = dto.q.trim().toLowerCase();
			if (dto.q.length() == 0) {
				dto.q = null;
			}
		}

		return userRepository.searchContact2(dto.q, dto.orgId, BussinessCommon.getClientId(), sort);
	}
}
