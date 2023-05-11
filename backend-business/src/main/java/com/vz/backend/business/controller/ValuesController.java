package com.vz.backend.business.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.Values;
import com.vz.backend.business.dto.ObjectFieldDto;
import com.vz.backend.business.dto.ValueDto;
import com.vz.backend.business.service.ValueService;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.service.IService;

@RestController
@RequestMapping("values/")
public class ValuesController extends BaseController<Values> {

	@Autowired
	ValueService vService;

	@Override
	public IService<Values> getService() {
		return null;
	}

	@PostMapping(value = "/addValues")
	public ResponseEntity<?> addValue(@RequestBody ObjectFieldDto<ValueDto> input) {
		ValueDto[] arr = input.getObjects();
		return new ResponseEntity<>(vService.saveValues(arr), HttpStatus.OK);
	}

	@GetMapping(value = "/getValues/{catId}/{formId}")
	public ResponseEntity<?> getValues(@PathVariable String catId, @PathVariable String formId) {
		return new ResponseEntity<>(vService.getValuesById(catId, formId), HttpStatus.OK);
	}

	@PostMapping(value = "/updateValues")
	public ResponseEntity<?> updateValues(@RequestBody ObjectFieldDto<ValueDto> input) {
		ValueDto[] arr = input.getObjects();
		return new ResponseEntity<>(vService.updateValues(arr), HttpStatus.OK);
	}

	@PostMapping(value = "/del/{catId}/{formId}")
	public ResponseEntity<?> delValues(@PathVariable String catId, @PathVariable String formId) {
		vService.del(catId, formId);
		return new ResponseEntity<>(null, HttpStatus.OK);
	}
}
