package com.vz.backend.core.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.core.config.Constant;
import com.vz.backend.core.domain.OutsideSystem;
import com.vz.backend.core.service.OutsideSystemService;

@RestController
@RequestMapping("/out-sys")
public class OutsideSystemController {

	@Autowired
	private OutsideSystemService outsideSystemService;

	@GetMapping("/all")
	public ResponseEntity<Page<OutsideSystem>> getAll(
			@RequestParam(required = false, defaultValue = "1") Integer page) {
		Pageable pageable = PageRequest.of(page - 1, Constant.NUMBER_OF_PAGE);
		return new ResponseEntity<>(outsideSystemService.findAll(pageable), HttpStatus.OK);
	}

	@GetMapping("/list")
	public ResponseEntity<List<OutsideSystem>> getList() {
		return new ResponseEntity<>(outsideSystemService.findAll(), HttpStatus.OK);
	}

	@PostMapping(value = "/add")
	public ResponseEntity<?> add(@RequestBody OutsideSystem data) {
		return new ResponseEntity<>(outsideSystemService.add(data), HttpStatus.OK);
	}

	@PostMapping(value = "/update/{id}")
	public ResponseEntity<?> update(@PathVariable Long id, @RequestBody OutsideSystem data) {
		return new ResponseEntity<>(outsideSystemService.update(id, data), HttpStatus.OK);
	}

	@PostMapping(value = "/control/{id}")
	public ResponseEntity<?> control(@PathVariable Long id, @RequestParam(required = false) Boolean active) {
		return new ResponseEntity<>(outsideSystemService.control(id, active), HttpStatus.OK);
	}
}
