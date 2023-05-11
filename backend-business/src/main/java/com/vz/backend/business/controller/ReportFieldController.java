package com.vz.backend.business.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.ReportField;
import com.vz.backend.business.service.ReportFieldService;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.dto.LabelValueDto;

@RestController
@RequestMapping("/report_field")
public class ReportFieldController {

	@Autowired
	ReportFieldService rpFieldsService;

	@GetMapping("/add/{type}")
	public ResponseEntity<List<ReportField>> createFields(@PathVariable DocumentTypeEnum type) { 
		return new ResponseEntity<>(rpFieldsService.saveByObjType(type), HttpStatus.OK);
	}
	
	@PostMapping("/update/{type}")
	public ResponseEntity<?> updateOrderNumber(@PathVariable DocumentTypeEnum type, @RequestBody List<ReportField> reportFields) { 
		return new ResponseEntity<>(rpFieldsService.updateOrderNumber(type, reportFields), HttpStatus.OK);
	}
	
	@GetMapping("/list/{type}")
	public ResponseEntity<List<ReportField>> listFields(@PathVariable DocumentTypeEnum type) {
		List<ReportField> rs = rpFieldsService.list(type);
		if(rs.isEmpty()) {
			rs = rpFieldsService.saveByObjType(type);
		}
		return new ResponseEntity<>(rs, HttpStatus.OK);
	}
	
	@GetMapping("/type-obj")
	public ResponseEntity<List<LabelValueDto<String>>> typeObj() {
		return new ResponseEntity<>(DocumentTypeEnum.get(DocumentTypeEnum.REPORT_FIELDS_CONFIGS), HttpStatus.OK);
	}
}
