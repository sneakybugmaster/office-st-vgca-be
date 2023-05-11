package com.vz.backend.business.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.hstl.Font;
import com.vz.backend.business.service.hstl.FontService;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.dto.LabelValueId;

@RestController
@RequestMapping("/font")
public class FontController {

	@Autowired
	private FontService fontService;

	@PostMapping(value = "/add")
	public ResponseEntity<Font> add(@RequestBody Font input) {
		return new ResponseEntity<>(fontService.add(input), HttpStatus.OK);
	}

	@PostMapping(value = "/update/{id}")
	public ResponseEntity<Font> update(@PathVariable Long id, @RequestBody Font input) {
		return new ResponseEntity<>(fontService.update(id, input), HttpStatus.OK);
	}
	
	@PostMapping(value = "/del/{id}")
	public ResponseEntity<Boolean> delete(@PathVariable Long id) {
		return new ResponseEntity<>(fontService.del(id), HttpStatus.OK);
	}

	@GetMapping("/detail/{id}")
	public ResponseEntity<Font> detail(@PathVariable Long id) {
		return new ResponseEntity<>(fontService.valid(id, Message.NOT_FOUND_FONT), HttpStatus.OK);
	}
	
	@GetMapping("/page/{page}")
	public ResponseEntity<Page<Font>> page(@PathVariable Integer page) {
		return new ResponseEntity<>(fontService.paging(page), HttpStatus.OK);
	}
	
	@GetMapping("/list")
	public ResponseEntity<List<LabelValueId<String>>> list() {
		return new ResponseEntity<>(fontService.list(), HttpStatus.OK);
	}
}
