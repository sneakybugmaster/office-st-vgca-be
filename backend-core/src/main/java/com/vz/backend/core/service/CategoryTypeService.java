package com.vz.backend.core.service;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.CategoryType;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.ICateroryTypeRepository;
import com.vz.backend.core.repository.IRepository;

@Service
public class CategoryTypeService extends BaseService<CategoryType> {

	@Autowired
	private ICateroryTypeRepository categoryTypeRepository;

	@Override
	public IRepository<CategoryType> getRepository() {
		return categoryTypeRepository;
	}

	public CategoryType findByClientIdAndName(Long clientId, String name) {
		return categoryTypeRepository.findByClientIdAndName(clientId, name);
	}

	public CategoryType findByClientIdAndCode(Long clientId, String categoryTypeCode) {
		return categoryTypeRepository.findByClientIdAndCode(clientId, categoryTypeCode);
	}

	public ListObjectDto<CategoryType> findByClientIdAndPage(Long clientId, Pageable page) {
		return BussinessCommon.paging(categoryTypeRepository.findClientId(clientId, page));
	}

	public List<CategoryType> findByClientIdAndCode(String[] codes) {
		return categoryTypeRepository.findByClientIdAndCodeAndActive(BussinessCommon.getClientId(), codes, true);
	}

	public List<CategoryType> getListCategoriesTypeByCode(String[] codes) {
		List<CategoryType> catTypeList = findByClientIdAndCode(codes);
		if (catTypeList == null) {
			throw new RestExceptionHandler(Message.ERROR_SYS);
		}
		return catTypeList;
	}
}
