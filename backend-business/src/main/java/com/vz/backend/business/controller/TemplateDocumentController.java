package com.vz.backend.business.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.google.common.net.HttpHeaders;
import com.vz.backend.business.domain.TemplateDocument;
import com.vz.backend.business.service.TemplateDocumentService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.CalendarStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.dto.LabelValueDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.core.service.RoleService;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/template")
public class TemplateDocumentController {

	@Autowired
	TemplateDocumentService templateService;
		@Autowired
	RoleService rService;
	
	@Autowired
	FilesStorageService storageService;

	@PostMapping(value = "/add")
	public ResponseEntity<List<TemplateDocument>> add(@RequestParam MultipartFile[] files,
			@RequestParam(defaultValue = "VAN_BAN_DI") DocumentTypeEnum type,
			@RequestParam(required = false) String tName) {
		return new ResponseEntity<>(templateService.add(files, type, tName), HttpStatus.OK);
	}

	@GetMapping("/all")
	public ResponseEntity<Page<TemplateDocument>> all(
			@RequestParam(value = "type", required = false) DocumentTypeEnum type,
			@RequestParam(value = "fileName", required = false) String name,
			@RequestParam(value = "size", required = false, defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", required = false, defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_NUMBER) int page) {
		Pageable pageable = BussinessCommon.castToPageable(page, size);
		name = BussinessCommon.convert(name);
		return new ResponseEntity<>(templateService.all(null, type, name, pageable), HttpStatus.OK);
	}
	
	@GetMapping("/list")
	public ResponseEntity<List<TemplateDocument>> list() {
		return new ResponseEntity<>(templateService.list(), HttpStatus.OK);
	}
	
	@GetMapping("/approve/{id}")
	public ResponseEntity<TemplateDocument> approve(@PathVariable Long id) {
		if (!rService.isAllowModule(BussinessCommon.getUser(), ModuleCodeEnum.ADMIN.getName() )) 
			throw new RestExceptionHandler(Message.ROLE_NOT_FOUND);
		return new ResponseEntity<>(templateService.update(id, CalendarStatusEnum.APPROVE), HttpStatus.OK);
	}

	@GetMapping("/status-enum")
	public ResponseEntity<List<LabelValueDto<String>>> getStatusEnum() {
		return new ResponseEntity<>(CalendarStatusEnum.get(), HttpStatus.OK);
	}
	
	@GetMapping("/download/{name:.+}")
	public ResponseEntity<Resource> getFile(@PathVariable String name) {
		Resource file = storageService.load(StringUtils.decodeFromUrl(name));
		if (file == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + file.getFilename() + "\"")
				.body(file);
	}
	
	/**
	 * using template file
	 * @param <T>
	 * @param id template
	 * @param type
	 * @return
	 */
	@GetMapping(value = "/use/{type}/{id}")
	public <T> ResponseEntity<T> use(@PathVariable Long id, @PathVariable DocumentTypeEnum type, @RequestParam String nName) {
		T t = templateService.use(id, type, nName);
		return new ResponseEntity<>(t, HttpStatus.OK);
	}
	
	@GetMapping("/draft/{type}")
	public <T> ResponseEntity<Page<T>> draft(@PathVariable DocumentTypeEnum type,
		@RequestParam(value = "size", required = false, defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_SIZE) int size,
		@RequestParam(value = "page", required = false, defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_NUMBER) int page) {
	Pageable pageable = BussinessCommon.castToPageable(page, size);
		return new ResponseEntity<>(templateService.draft(type, pageable), HttpStatus.OK);
	}
	
	/**
	 * update file attachment
	 * keep old name file
	 *  
	 * @param <T>
	 * @param id of attachment
	 * @param type
	 * @return
	 */
	@PostMapping(value = "/update/{type}/{aId}")
	public <T> ResponseEntity<T> update(@PathVariable Long aId, @PathVariable DocumentTypeEnum type,
			@RequestParam MultipartFile file) {
		T t = templateService.update(aId, type, file);
		return new ResponseEntity<>(t, HttpStatus.OK);
	}
	
	@GetMapping(value = "/update/{type}/{aId}/{objId}")
	public <T> ResponseEntity<T> update(@PathVariable Long objId, @PathVariable Long aId, @PathVariable DocumentTypeEnum type) {
		T t = templateService.update(objId, aId, type);
		return new ResponseEntity<>(t, HttpStatus.OK);
	}
	
	@GetMapping(value = "/del/{type}/{aId}")
	public ResponseEntity<?> del(@PathVariable Long aId, @PathVariable DocumentTypeEnum type) {
		return new ResponseEntity<>(templateService.del(aId, type), HttpStatus.OK);
	}
}
