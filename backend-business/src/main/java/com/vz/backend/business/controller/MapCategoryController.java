package com.vz.backend.business.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.MapCategory;
import com.vz.backend.business.service.MapCategoryService;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.service.IService;

@RestController
@RequestMapping("/map_category")
public class MapCategoryController extends BaseController<MapCategory> {
	@Autowired
	private MapCategoryService mapCategoryService;

	@Override
	public IService<MapCategory> getService() {
		return mapCategoryService;
	}
}
