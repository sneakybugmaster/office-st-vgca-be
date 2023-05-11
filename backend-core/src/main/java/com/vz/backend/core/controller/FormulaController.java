package com.vz.backend.core.controller;

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

import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Formula;
import com.vz.backend.core.domain.FormulaDetail;
import com.vz.backend.core.dto.FormulaDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.FormulaDetailService;
import com.vz.backend.core.service.FormulaService;

@RequestMapping("/formula")
@RestController
public class FormulaController {

	@Autowired
	FormulaService fService;
	
	@Autowired
	FormulaDetailService fDetailService;
	
	/**
	 * for add and update
	 * @param f
	 * @return
	 */
	@PostMapping("/add")
	public ResponseEntity<?> add(@RequestBody Formula f) {
		return new ResponseEntity<>(fService.add(f), HttpStatus.OK);
	}
	
	@PostMapping("/detail/add")
	public ResponseEntity<?> addDetail(@RequestBody FormulaDetail f) {
		return new ResponseEntity<>(fDetailService.add(f), HttpStatus.OK);
	}
	
	/**
	 * delete formula
	 * @param f
	 * @return
	 */
	@GetMapping("/del/{id}")
	public ResponseEntity<?> delAll(@PathVariable Long id) {
		fService.valid(id, Message.NOT_FOUND_FORMULA);
		try {
			fService.deleteById(id);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.FORMULA_USING);
		}
		return new ResponseEntity<>(true, HttpStatus.OK);
	}
	
	/**
	 * for delete formula detail
	 * @param id
	 * @return
	 */
	@GetMapping("/detail/del/{id}")
	public ResponseEntity<Boolean> delDetail(@PathVariable Long id) {
		fDetailService.valid(id, Message.NOT_FOUND_FORMULA);
		fDetailService.deleteById(id);
		return new ResponseEntity<>(true, HttpStatus.OK);
	}
	
	@GetMapping("/{id}")
	public ResponseEntity<?> details(@PathVariable Long id) {
		return new ResponseEntity<>(fService.valid(id, Message.NOT_FOUND_FORMULA), HttpStatus.OK);
	}

	@PostMapping("/list")
	public ResponseEntity<?> list(@RequestBody FormulaDto dto) {
		return new ResponseEntity<>(fService.list(dto), HttpStatus.OK);
	}
	
	@GetMapping("/all")
	public ResponseEntity<List<FormulaDto>> all() {
		return new ResponseEntity<>(fService.list(), HttpStatus.OK);
	}
}
