package com.vz.backend.business.controller;

import javax.websocket.server.PathParam;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.AssignTask;
import com.vz.backend.business.repository.IAssignTaskRepository;
import com.vz.backend.business.service.FieldService;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.service.IService;

@RestController
@RequestMapping("/assign_task")
public class AssignTaskController extends BaseController<AssignTask> {

	@Override
	public IService<AssignTask> getService() {
		return null;
	}

	@Autowired
	IAssignTaskRepository iAssignTaskRepository;

	@Autowired
	FieldService feildService;

	@GetMapping(value = "/getList")
	public ResponseEntity<?> getList(@PathParam(value = "page") String page) {
		/*
		 * List<ObjectFieldDto> taskDtoList =
		 * feildService.getList(iAssignTaskRepository.findByClientId(feildService.
		 * getClient()), feildService.getListByCatId(CategoryEnum.DOCUMENT.getValue()));
		 * Integer pageL = page != null && page.length() > 0 ? Integer.parseInt(page) :
		 * null;
		 */

		return new ResponseEntity<>(page, HttpStatus.OK);
	}

}
