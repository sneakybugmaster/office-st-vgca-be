package com.vz.backend.business.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.service.ClericalOrgService;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.service.UserService;

@RestController
@RequestMapping("/clerical")
public class ClericalOrgController {
	enum SortBy {
		UPDATE_DATE("updateDate"), fullName("fullName"), ACTIVE("active");

		private String field;

		private SortBy(String field) {
			this.field = field;
		}
	}
	
	@Autowired
	private UserService userService;
	
	@Autowired
	private ClericalOrgService clericalOrgService;
	
	private void checkPermission() {
//		if (userService.checkUserIdByModuleCodeAndClientId(BussinessCommon.getUserId(),
//				Arrays.asList(ModuleCodeEnum.DOCUMENT_BOOK.getName()), BussinessCommon.getClientId()))
//			return;
//		throw new RestForbidden("Bạn không có quyền truy cập vào sổ văn bản");
	}
	
	@PostMapping("/addOrg")
	public ResponseEntity<?> addOrg(@RequestParam Long userId, @RequestBody List<Long> orgIds) {
		checkPermission();
		clericalOrgService.add(userId, orgIds);
		return new ResponseEntity<>(true, HttpStatus.OK);
	}
	
//	@PostMapping("/updateOrg")
//	public ResponseEntity<?> updateOrg(@RequestParam Long userId, @RequestBody List<Long> orgIds) {
//		checkPermission();
//		clericalOrgService.add(userId, orgIds);
//		return new ResponseEntity<>(true, HttpStatus.OK);
//	}
	
	@GetMapping(value = "/getClericalOrg")
	public ResponseEntity<?> getClericalOrg(@RequestParam Long userId) {
		checkPermission();
		return new ResponseEntity<>(clericalOrgService.getClericalOrg(userId), HttpStatus.OK);
	}

	@GetMapping(value = "/getClericalByOrgId")
	public ResponseEntity<?> getClericalOrgByOrgId(@RequestParam Long orgId) {
		checkPermission();
		return new ResponseEntity<>(clericalOrgService.getClericalByOrgIdAndClientId(orgId), HttpStatus.OK);
	}
	
	
	@GetMapping("/getClerical")
	public ResponseEntity<?> getClerical(@RequestParam(required = false) DocumentTypeEnum docType,
			@RequestParam(defaultValue = "1") int page, 
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(required = false) String name, @RequestParam(required = false) Long orgId) {
		Pageable pageable = PageRequest.of(page - 1, size);
		return new ResponseEntity<>(clericalOrgService.getClerical(name, orgId, docType, pageable), HttpStatus.OK);
	}

	@GetMapping(path = "/getTreeOrgByChil", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> getTreeOrgByChil() {
		return new ResponseEntity<>(clericalOrgService.getTreeOrgByChil(), HttpStatus.OK);
	}
}
