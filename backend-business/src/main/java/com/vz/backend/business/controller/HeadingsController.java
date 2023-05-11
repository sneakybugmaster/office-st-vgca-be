package com.vz.backend.business.controller;

import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.vz.backend.core.util.StreamUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.DateTimeFormat.ISO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.vz.backend.business.domain.hstl.Headings;
import com.vz.backend.business.service.hstl.HeadingsService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

@RestController
@RequestMapping("/headings")
public class HeadingsController {

	@Autowired
	private HeadingsService headingsService;
	
	@PostMapping("/add")
	public ResponseEntity<Headings> createHeadings(@RequestBody Headings input) {
		headingsService.validRole();
		return new ResponseEntity<>(headingsService.add(input), HttpStatus.OK);
	}
	
	@PostMapping("/edit/{id}")
	public ResponseEntity<Headings> updateHeadings(@PathVariable Long id,  @RequestBody Headings input) {
		headingsService.validRole();
		return new ResponseEntity<>(headingsService.update(id, input), HttpStatus.OK);
	}
	
	@PostMapping("/del/{id}")
	public ResponseEntity<Boolean> delHeadings(@PathVariable Long id) {
		headingsService.validRole();
		return new ResponseEntity<>(headingsService.del(id), HttpStatus.OK);
	}
	
	@GetMapping("/tree")
	public ResponseEntity<List<Headings>> getHeadingsTree() {
		return new ResponseEntity<>(headingsService.getHeadingsTree(), HttpStatus.OK);
	}
	
	@GetMapping("/gen/{id}")
	public ResponseEntity<Set<Long>> getHeadingsGen(@PathVariable Long id) {
		return new ResponseEntity<>(headingsService.getNextGen(id, new HashSet<>()), HttpStatus.OK);
	}
	
	@GetMapping("/folder/tree")
	public ResponseEntity<List<Headings>> getHeadingsFolderTree(@RequestParam(required = false) String text,
			@RequestParam(required = false) Integer yearFolders, @RequestParam(required = false) String typeFolders,
			@RequestParam(required = false) Long maintenance,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date to,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date from) {
		text = BussinessCommon.convert(text);
		typeFolders = BussinessCommon.convert(typeFolders);
		from = DateTimeUtils.handleSubmit(from);
		to = DateTimeUtils.getEndDate(to);
		maintenance = BussinessCommon.convert(maintenance);
		return new ResponseEntity<>(
				headingsService.buildHeadingsFolderTree(text, yearFolders, typeFolders, maintenance, from, to),
				HttpStatus.OK);
	}
	
	@GetMapping("/record/export")
	public ResponseEntity<StreamingResponseBody> exportHeadings(@RequestParam(required = false) String text,
			@RequestParam(required = false) Integer yearFolders, @RequestParam(required = false) String typeFolders,
			@RequestParam(required = false) Long maintenance,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date to,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date from) {
		StreamingResponseBody stream = outputStream -> {
			try {
				headingsService.export(outputStream, text, yearFolders, typeFolders, maintenance, from, to);
			} finally {
				StreamUtils.closeOutputStream(outputStream);
			}
		};
		return new ResponseEntity<>(stream, HttpStatus.OK);
	}
	
//	@GetMapping("/level/list")
//	public ResponseEntity<List<Headings>> getByLevel() {
//		return new ResponseEntity<>(headingsService.treeToList(), HttpStatus.OK);
//	}
//	
//	@GetMapping("/record/tree")
//	public ResponseEntity<List<RecordHeadings>> getRecordHeadings() {
//		return new ResponseEntity<>(headingsService.getRecordHeadings(), HttpStatus.OK);
//	}
	
	@GetMapping("/record/folder/export")
	public ResponseEntity<StreamingResponseBody> exportFolders(@RequestParam(required = false) String text,
			@RequestParam(required = false) Integer yearFolders, @RequestParam(required = false) String typeFolders,
			@RequestParam(required = false) Long maintenance,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date to,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date from) {
		StreamingResponseBody stream = outputStream -> {
			try {
				headingsService.exportFolders(outputStream, text, yearFolders, typeFolders, maintenance, from, to);
			} finally {
				StreamUtils.closeOutputStream(outputStream);
			}
		};
		return new ResponseEntity<>(stream, HttpStatus.OK);
	}
	
	@GetMapping("/record/doc/export")
	public ResponseEntity<StreamingResponseBody> exportDocs(
			@RequestParam() Long folderId) {
		StreamingResponseBody stream = outputStream -> {
			try {
				headingsService.exportDocs(outputStream, folderId);
			} finally {
				StreamUtils.closeOutputStream(outputStream);
			}
		};
		return new ResponseEntity<>(stream, HttpStatus.OK);
	}
}
