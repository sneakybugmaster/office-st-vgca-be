package com.vz.backend.core.controller;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.core.dto.OrgGroupDto;
import com.vz.backend.core.dto.UserInfoDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestClientException;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.domain.Group;
import com.vz.backend.core.dto.GroupWithListUserDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.GroupService;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/group")
public class GroupController {
	enum SortBy {
		UPDATEDATE("updateDate"),
		NAME("name"),
		DESCRIPTION("description"),
		ACTIVE("active");

		private String field;

		private SortBy(String field) {
			this.field = field;
		}
	}
	
	@Autowired
	private GroupService groupService;
	
	@PostMapping("/add")
	public ResponseEntity<?> addGroup(@RequestBody GroupWithListUserDto input) {
		checkPermission();
		if (StringUtils.isNullOrEmpty(input.getName(), true))
			throw new RestExceptionHandler("Tên nhóm không được để trống");
		Group gr = groupService.save(new Group(input.getName(), input.getDescription(), input.getNodeId()));
		groupService.add(gr.getId(), input.getListUser());
		return new ResponseEntity<>(gr, HttpStatus.OK);
	}

	@PostMapping("/update")
	public ResponseEntity<?> updateGroup(@RequestBody GroupWithListUserDto input) {
		checkPermission();
		Group gr = groupService.findByClientIdAndId(BussinessCommon.getClientId(), input.getId());
		if (gr == null)
			throw new RestClientException("Không tìm thấy nhóm phù hợp...");
		gr.setName(input.getName());
		gr.setDescription(input.getDescription());
		gr = groupService.save(gr);
		groupService.add(gr.getId(), input.getListUser());
		return new ResponseEntity<>(gr, HttpStatus.OK);
	}
	
	@PostMapping("/addUser")
	public ResponseEntity<?> addUser(@RequestParam Long groupId, @RequestParam List<Long> listUser) {
		checkPermission();
		return new ResponseEntity<>(groupService.add(groupId, listUser), HttpStatus.OK);
	}
	
	@PostMapping("/delete")
	public ResponseEntity<?> delete(@RequestParam Long groupId) {
		checkPermission();
		return new ResponseEntity<>(groupService.delete(groupId), HttpStatus.OK);
	}
	
	@PostMapping("/active")
	public ResponseEntity<?> active(@RequestParam Long groupId) {
		checkPermission();
		return new ResponseEntity<>(groupService.active(groupId), HttpStatus.OK);
	}
	
	@PostMapping("/deactive")
	public ResponseEntity<?> deactive(@RequestParam Long groupId) {
		checkPermission();
		return new ResponseEntity<>(groupService.deactive(groupId), HttpStatus.OK);
	}
	
	@GetMapping(value = "/getAllGroup")
	public ResponseEntity<?> getAllGroup(@RequestParam(required = false) Boolean active,
			@RequestParam(required = false) String groupName,
			@RequestParam(required = false) String description,
			@RequestParam(required = false) Long nodeId,
			@RequestParam(defaultValue = "UPDATEDATE") SortBy sortBy,
			@RequestParam(defaultValue = "ASC") Direction direction,
			@RequestParam(defaultValue = "" + Constant.NUMBER_OF_PAGE) int size,
			@RequestParam(defaultValue = "1") int page) {
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(groupService.getAllGroup(active, groupName, description, nodeId, pageable), HttpStatus.OK);
	}
	
	private void checkPermission() {
		return;
	}

	@GetMapping(value = "/search-group")
	public ResponseEntity<List<OrgGroupDto>> searchName(@RequestParam("q") String q) {
		List<OrgGroupDto> result = groupService.searchName(q);
		return new ResponseEntity<>(result, HttpStatus.OK);
	}

	@GetMapping(value = "/pageUserInGroup")
	public ResponseEntity<Page<UserInfoDto>> searchName(@RequestParam("groupId") Long groupId, @RequestParam("nameSearch") String nameSearch,
														@RequestParam(defaultValue = "" + Constant.NUMBER_OF_PAGE) int size,
														@RequestParam(defaultValue = "1") int page) {
		Pageable pageable = PageRequest.of(page - 1, size);
		Page<UserInfoDto> result = groupService.pageUserInGroup(groupId, nameSearch, pageable);
		return new ResponseEntity<>(result, HttpStatus.OK);
	}
}
