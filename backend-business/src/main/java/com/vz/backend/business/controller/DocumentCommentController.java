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

import com.vz.backend.business.domain.DocumentComment;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.service.DocumentCommentService;
import com.vz.backend.business.service.DocumentService;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.IService;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

@RestController
@RequestMapping("/comment_doc")
public class DocumentCommentController extends BaseController<DocumentComment> {
	@Autowired
	private DocumentCommentService commentService;

	@Autowired
	DocumentService docService;

	@Autowired
	CategoryService categoryService;

	@Override
	public IService<DocumentComment> getService() {
		return commentService;
	}

	@PostMapping(value = "/addDoc")
	public ResponseEntity<DocumentComment> addDoc(@RequestBody DocumentComment cmt) {
		commentService.validCmt(cmt);

		// document is done not comment
		Documents doc = docService.validDocId(cmt.getDocId());
		DocumentComment dcmt = commentService.saveCmt(cmt);
		docService.updateResolveFile(doc);
		return new ResponseEntity<>(dcmt, HttpStatus.OK);
	}

	@GetMapping(value = "/getListByDocId/{docId}")
	public ResponseEntity<?> getListByDocId(@PathVariable Long docId) {
		docService.checkPermission(docId);
		return new ResponseEntity<>(commentService.getListByDocId(docId), HttpStatus.OK);
	}

	@GetMapping("/export/{commentId}")
	public ResponseEntity<StreamingResponseBody> exportComment(@PathVariable(value = "commentId") Long commentId) {
		StreamingResponseBody stream = outputStream -> commentService.exportComment(outputStream, commentId);
		return new ResponseEntity<>(stream, HttpStatus.OK);
	}

	@Override
	@PostMapping(value = "/delete/{id}")
	public ResponseEntity<?> delete(@PathVariable Long id) {
		try {
			commentService.deleteComment(id);
			return ResponseEntity.status(HttpStatus.OK).build();
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}
	
	@GetMapping(value = "/load/{id}")
	public ResponseEntity<DocumentComment> getById(@PathVariable Long id){
		return new ResponseEntity<>(commentService.load(id), HttpStatus.OK);
	}
	
	@PostMapping(value = "/edit/{id}")
	public ResponseEntity<DocumentComment> getById(@PathVariable Long id, @RequestParam String comment){
		return new ResponseEntity<>(commentService.update(id, comment), HttpStatus.OK);
	}
}
