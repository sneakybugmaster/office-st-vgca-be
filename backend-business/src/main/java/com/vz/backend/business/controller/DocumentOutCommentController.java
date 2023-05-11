package com.vz.backend.business.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.DocumentOutComment;
import com.vz.backend.business.service.DocumentOutCommentService;
import com.vz.backend.business.service.DocumentOutService;
import com.vz.backend.core.service.IService;

@RestController
@RequestMapping("/doc_out_comment")
public class DocumentOutCommentController {

	@Autowired
	DocumentOutCommentService docOutCmtService;
	
	@Autowired
	DocumentOutService docOutService;

	public IService<DocumentOutComment> getService() {
		return docOutCmtService;
	}

	@PostMapping("/addComment/{docId}")
	public ResponseEntity<?> addComment(@PathVariable Long docId, @RequestBody String cmt) {
		return new ResponseEntity<>(docOutCmtService.saveCmtAndUpdateStatus(docId, cmt), HttpStatus.OK);
	}

	@PostMapping("/addComment")
	public ResponseEntity<?> addComment(@RequestBody DocumentOutComment cmt) {
		return new ResponseEntity<>(docOutCmtService.saveCmtAndUpdateStatus(cmt), HttpStatus.OK);
	}

	@GetMapping("/getListByDocId/{docId}")
	public ResponseEntity<?> getListByDocId(@PathVariable Long docId) {
//		docOutService.validatePermission(docId);
		return new ResponseEntity<>(docOutCmtService.getListByDocId(docId), HttpStatus.OK);
	}
	
	
	@GetMapping(value = "/load/{id}")
	public ResponseEntity<DocumentOutComment> getById(@PathVariable Long id){
		return new ResponseEntity<>(docOutCmtService.load(id), HttpStatus.OK);
	}
	
	@PostMapping(value = "/edit/{id}")
	public ResponseEntity<DocumentOutComment> getById(@PathVariable Long id, @RequestParam String comment){
		return new ResponseEntity<>(docOutCmtService.update(id, comment), HttpStatus.OK);
	}
}
