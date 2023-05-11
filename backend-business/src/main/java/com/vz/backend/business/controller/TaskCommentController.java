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

import com.vz.backend.business.domain.TaskComment;
import com.vz.backend.business.service.DocumentService;
import com.vz.backend.business.service.TaskCommentService;
import com.vz.backend.core.service.CategoryService;

@RestController
@RequestMapping("/taskComment")
public class TaskCommentController {
	@Autowired
	private TaskCommentService service;

	@Autowired
	DocumentService docService;

	@Autowired
	CategoryService categoryService;

	@PostMapping(value = "/add")
	public ResponseEntity<?> addDoc(@RequestBody TaskComment cmt) {
		return new ResponseEntity<>(service.save(cmt), HttpStatus.OK);
	}

	@GetMapping(value = "/getComment/{taskId}")
	public ResponseEntity<?> getComment(@PathVariable Long taskId) {
		return new ResponseEntity<>(service.getComments(taskId), HttpStatus.OK);
	}

}
