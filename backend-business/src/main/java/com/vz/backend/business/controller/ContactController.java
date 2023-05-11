package com.vz.backend.business.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.dto.ContactSearchDto;
import com.vz.backend.business.service.ContactService;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.dto.ContactDto;
import com.vz.backend.core.dto.SearchDto;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.service.RoleService;

@RestController
@RequestMapping("/contact")
public class ContactController {

	@Autowired
	private ContactService contactService;

	@Autowired
	private RoleService roleService;
	
	@PostMapping("/all")
	public ResponseEntity<Page<ContactDto>> all(@RequestBody SearchDto dto) {
		requirePermission();
		return new ResponseEntity<>(contactService.all(dto), HttpStatus.OK);
	}

	@PostMapping("/search")
	public ResponseEntity<Page<ContactDto>> search(@RequestBody ContactSearchDto dto) {
		requirePermission();
		return new ResponseEntity<>(contactService.search(dto), HttpStatus.OK);
	}

	@PostMapping("/search2")
	public ResponseEntity<List<ContactDto>> search2(@RequestBody ContactSearchDto dto) {
		requirePermission();
		return new ResponseEntity<>(contactService.search2(dto), HttpStatus.OK);
	}

	private void requirePermission() {
		if (true || roleService.existUserInModule(ModuleCodeEnum.PROCESS.getName())) {
			return;
		}
		throw new RestForbidden("No permission for this action");
	}
}
