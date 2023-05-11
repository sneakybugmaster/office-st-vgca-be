package com.vz.backend.business.controller;

import java.util.List;

import javax.websocket.server.PathParam;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.DocumentInTracking;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.dto.FollowDto;
import com.vz.backend.business.service.DocumentInTrackingService;
import com.vz.backend.business.service.DocumentService;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.service.IService;

@RestController
@RequestMapping("/doc_in_tracking")
public class DocumentInTrackingController extends BaseController<DocumentInTracking> {

	@Autowired
	DocumentInTrackingService trackingService;

	@Autowired
	DocumentService docService;

	@Override
	public IService<DocumentInTracking> getService() {
		return trackingService;
	}

	@GetMapping(value = "/follow/{docId}")
	public ResponseEntity<?> listTracking(@PathVariable Long docId, @PathParam(value = "page") Integer page) {
		Documents doc = docService.validDocId(docId);
		return new ResponseEntity<>(trackingService.listFollow(docId, page, doc), HttpStatus.OK);
	}
	
	@GetMapping(value = "/all/{docId}")
	public ResponseEntity<List<FollowDto>> all(@PathVariable Long docId) {
		docService.validDocId(docId);
		return new ResponseEntity<>(trackingService.all(docId), HttpStatus.OK);
	}
}
