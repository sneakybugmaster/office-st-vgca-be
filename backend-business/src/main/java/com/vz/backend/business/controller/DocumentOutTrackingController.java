package com.vz.backend.business.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.dto.FollowDto;
import com.vz.backend.business.service.DocumentOutTrackingService;

@RestController
@RequestMapping("/doc_out_tracking")
public class DocumentOutTrackingController {

	@Autowired
	DocumentOutTrackingService docOutTrackingService;

	@GetMapping("/getTracking/{docId}")
	public ResponseEntity<?> listTracking(@PathVariable Long docId,
			@RequestParam(value = "page", defaultValue = "1") Integer page) {
		return new ResponseEntity<>(docOutTrackingService.listTracking(docId, page), HttpStatus.OK);
	}
	
	@GetMapping("/all/{docId}")
	public ResponseEntity<List<FollowDto>> all(@PathVariable Long docId) {
		return new ResponseEntity<>(docOutTrackingService.all(docId), HttpStatus.OK);
	}
}
