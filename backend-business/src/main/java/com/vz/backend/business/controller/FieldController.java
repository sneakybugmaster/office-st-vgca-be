package com.vz.backend.business.controller;

import javax.websocket.server.PathParam;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.Fields;
import com.vz.backend.business.dto.FieldDto;
import com.vz.backend.business.dto.ObjectFieldDto;
import com.vz.backend.business.repository.IFieldsRepository;
import com.vz.backend.business.service.FieldService;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.service.IService;

@RestController
@RequestMapping("field/")
public class FieldController extends BaseController<Fields> {

	@Override
	public IService<Fields> getService() {
		return null;
	}

	@Autowired
	FieldService fieldService;

	@Autowired
	IFieldsRepository fieldRepository;

	@PostMapping("/updateField")
	public ResponseEntity<?> updateField(@RequestBody ObjectFieldDto<FieldDto> input) {
		return new ResponseEntity<>(fieldService.update(input), HttpStatus.OK);
	}

	@PostMapping("/delField")
	public ResponseEntity<?> delField(@RequestBody ObjectFieldDto<FieldDto> input) {
		fieldService.del(input);
		return new ResponseEntity<>(input, HttpStatus.OK);
	}

	@PostMapping(value = "/addField")
	public ResponseEntity<?> addFields(@RequestBody ObjectFieldDto<FieldDto> input) {
		return new ResponseEntity<>(fieldService.save(input), HttpStatus.OK);
	}

	@GetMapping(value = "/listField")
	public ResponseEntity<?> listField(@PathParam(value = "catId") String catId) {
		return new ResponseEntity<>(fieldService.getListByClientIdAndCatId(catId), HttpStatus.OK);
	}
}
