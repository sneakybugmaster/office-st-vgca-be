package com.vz.backend.core.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.dto.DelegateFlowDto;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.service.DelegateFlowService;
import com.vz.backend.core.service.RoleService;

@RestController
@RequestMapping("/delegate_flow")
public class DelegateFlowController {

	enum SortBy {
		UPDATE_DATE("updateDate"), FROM_POSITION("fromPositionModel.name"), TO_POSITION("toPositionModel.name");

		private String field;

		private SortBy(String field) {
			this.field = field;
		}
	}

	@Autowired
	DelegateFlowService delegateFlowService;

	@Autowired
	private RoleService roleService;

	@PostMapping("/add")
	public ResponseEntity<?> create(@RequestParam Long from, @RequestParam Long to) {
		requirePermission();
		return new ResponseEntity<>(delegateFlowService.save(from, to), HttpStatus.OK);
	}

	@GetMapping("/list")
	public ResponseEntity<Page<DelegateFlowDto>> list(@RequestParam(defaultValue = "1") int page,
			@RequestParam(defaultValue = "UPDATE_DATE") SortBy sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size) {
		requirePermission();
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(delegateFlowService.list(pageable), HttpStatus.OK);
	}

	@PostMapping("/delete/{id}")
	public ResponseEntity<?> delete(@PathVariable Long id) {
		requirePermission();
		delegateFlowService.delete(id);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	private void requirePermission() {
		if (roleService.existUserInModule(ModuleCodeEnum.DELEGATE_FLOW.getName())) {
			return;
		}
		throw new RestForbidden(Message.NO_PERMISSION);
	}
}
