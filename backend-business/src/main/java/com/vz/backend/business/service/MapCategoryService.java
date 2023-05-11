package com.vz.backend.business.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.MapCategory;
import com.vz.backend.business.repository.IMapCategoryRepository;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class MapCategoryService extends BaseService<MapCategory> {
	@Autowired
	private IMapCategoryRepository progressRepository;

	@Override
	public IRepository<MapCategory> getRepository() {
		return progressRepository;
	}
}
