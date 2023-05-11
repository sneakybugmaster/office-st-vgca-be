package com.vz.backend.core.controller;

import java.util.List;

import javax.websocket.server.PathParam;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.domain.CategoryType;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.service.CategoryTypeService;
import com.vz.backend.core.service.IService;
import com.vz.backend.core.service.RoleService;

@RestController
@RequestMapping("/category-type")
public class CategoryTypeController {// extends BaseController<CategoryType>{
	@Autowired
	private CategoryTypeService categoryTypeService;

	// @Override
	public IService<CategoryType> getService() {
		return categoryTypeService;
	}

	@Autowired
	private RoleService roleService;

	private void checkPermission() {
		if (roleService.existUserInModule(ModuleCodeEnum.CATEGORY.getName())) {
			return;
		}
		throw new RestForbidden("Bạn không có quyền truy cập vào danh mục");
	}

	// @Override
//	public Long getClientId() {
//		User user =  getCurrentUser();
//		if(user != null) {
//			return user.getClientId();
//		}
//		throw new RestExceptionHandler(Message.NOT_FOUND_CLIENT);
//	}

	@PostMapping("/add")
	public ResponseEntity<?> create(@RequestBody CategoryType input) {
		checkPermission();
		return new ResponseEntity<>(categoryTypeService.save(input), HttpStatus.OK);
	}

	@PostMapping("/update/{id}")
	public ResponseEntity<?> update(@PathVariable Long id, @RequestBody CategoryType input) {
		CategoryType data = getService().findByClientIdAndId(BussinessCommon.getClientId(), id);
		if (data != null) {
			input.setId(data.getId());
		}
		data = getService().save(input);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@GetMapping("/search/{name}")
	public ResponseEntity<CategoryType> searchName(@PathVariable String name) {
		try {
			CategoryType category = categoryTypeService.findByClientIdAndName(BussinessCommon.getClientId(), name);
			if (category != null) {
				return new ResponseEntity<>(category, HttpStatus.OK);
			}
			return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/sadmin/getAll/{clientid}")
	public ResponseEntity<?> getByClientIdParam(@PathVariable long clientid) {
		List<CategoryType> data = getService().findByClientId(clientid);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@GetMapping("/sadmin/getAll/{clientid}/{page}")
	public ResponseEntity<?> getByClientId(@PathVariable long clientid, @PathVariable int page) {
		List<CategoryType> data = getService().findByClientId(clientid, page);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@GetMapping("/getAllSortAndPage/{direction}/{column}")
	public ResponseEntity<?> getByClientIdAndSortAndPage(@PathVariable String direction, @PathVariable String column,
			@PathParam(value = "page") String page, @RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size) {
		Sort sort;
		if (direction.equals("ASC")) {
			sort = Sort.by(Direction.ASC, column);
		} else {
			sort = Sort.by(Direction.DESC, column);
		}

		Integer pageL = BussinessCommon.setPageNumber(page);

		Pageable pageable = PageRequest.of(pageL - 1, size, sort);
		ListObjectDto<CategoryType> data = categoryTypeService.findByClientIdAndPage(BussinessCommon.getClientId(),
				pageable);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}
	
	@GetMapping("/{code}")
	public ResponseEntity<?> getByCode(@PathVariable String code) {
		CategoryType categoryType = categoryTypeService.findByClientIdAndCode(BussinessCommon.getClientId(), code);
		if (categoryType == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_CATEGORY);
		}
		return new ResponseEntity<>(categoryType, HttpStatus.OK);
	}
}
