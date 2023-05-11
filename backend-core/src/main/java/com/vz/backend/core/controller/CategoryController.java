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

import com.vz.backend.core.auth.SecurityContext;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.CategoryType;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.CategoryTypeService;
import com.vz.backend.core.service.IService;
import com.vz.backend.core.service.RoleService;

@RestController
@RequestMapping("/categories")
public class CategoryController {

	enum SortBy {
		UPDATE_DATE("updateDate"), ORDER("order"), NAME("name"), ID("id");

		private String field;

		private SortBy(String field) {
			this.field = field;
		}
	}

	@Autowired
	private CategoryService categoryService;

	@Autowired
	private CategoryTypeService categoryTypeService;

	@Autowired
	private RoleService roleService;

	private void checkPermission() {
		if (roleService.existUserInModule(ModuleCodeEnum.CATEGORY.getName())) {
			return;
		}
		throw new RestForbidden("Bạn không có quyền truy cập vào danh mục");
	}

	public IService<Category> getService() {
		return categoryService;
	}

	protected User getCurrentUser() {
		return SecurityContext.getCurrentUser();
	}

	public Long getClientId() {
		User user = getCurrentUser();
		if (user != null) {
			return user.getClientId();
		}
		throw new RestExceptionHandler(Message.NOT_FOUND_CLIENT);
	}

	@GetMapping("/search/{name}")
	public ResponseEntity<Category> searchName(@PathVariable String name) {
		try {
			Category categoryItem = categoryService.findByClientIdAndName(getClientId(), name);
			if (categoryItem != null) {
				return new ResponseEntity<>(categoryItem, HttpStatus.OK);
			}
			return ResponseEntity.status(HttpStatus.NOT_FOUND).build();
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/getAllByCategoryTypeId/{categoryTypeId}")
	public ResponseEntity<List<Category>> getAllByCategoryTypeId(@PathVariable long categoryTypeId) {
		try {
			List<Category> list = categoryService.findByClientIdAndCategoryTypeId(getClientId(), categoryTypeId,
					Sort.by(Direction.ASC, "order"));
			return new ResponseEntity<>(list, HttpStatus.OK);
		} catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/getAllByCategoryTypeIdPaging/{categoryTypeId}")
	public ResponseEntity<?> getAllByCategoryTypeIdPaging(@RequestParam(defaultValue = "1") int page,
			@RequestParam(defaultValue = "ORDER") SortBy sortBy,
			@RequestParam(defaultValue = "ASC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size, @PathVariable Long categoryTypeId,
			@PathParam(value = "name") String name, @PathParam(value = "active") Boolean active,
			@PathParam(value = "id") Long id) {
		try {
			Long idL = id != null && id.toString().length() > 0 ? id : null;
			String nameL = name != null && name.length() > 0 ? name : null;
			Long categoryTypeIdL = categoryTypeId != null && categoryTypeId.toString().length() > 0 ? categoryTypeId
					: null;
			Boolean activeL = active != null ? active : null;

			Sort sort = Sort.by(direction, sortBy.field);
			Pageable pageable = PageRequest.of(page - 1, size, sort);
			return new ResponseEntity<>(
					categoryService.findByClientIdAndCategoryTypeId(nameL, activeL, idL, categoryTypeIdL, pageable),
					HttpStatus.OK);
		} catch (Exception e) {
			e.printStackTrace();
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/getAllByCategoryTypeCode/{categoryTypeCode}")
	public ResponseEntity<List<Category>> getAllByCategoryTypeCode(@PathVariable String categoryTypeCode) {
		try {
			CategoryType categoryType = categoryTypeService.findByClientIdAndCode(getClientId(), categoryTypeCode);
			if (categoryType == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_CATEGORY);
			}
			List<Category> list = categoryService.findByClientIdAndCategoryTypeIdAndActive(getClientId(),
					categoryType.getId(), true, Sort.by(Direction.ASC, "order"));
			return new ResponseEntity<>(list, HttpStatus.OK);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@GetMapping("/findPosition/{orgId}")
	public ResponseEntity<?> findPosition(@PathParam(value = "page") String page, @PathVariable Long orgId) {
		Integer pageL = BussinessCommon.setPageNumber(page);
		return new ResponseEntity<>(BussinessCommon.getListByPageNumber(categoryService.findPosition(orgId), pageL),
				HttpStatus.OK);
	}

	@PostMapping("/update/{id}")
	public ResponseEntity<?> update(@PathVariable Long id, @RequestBody Category input) {
		checkPermission();
		return new ResponseEntity<>(categoryService.update(input, id), HttpStatus.OK);
	}

	@PostMapping("/add")
	public ResponseEntity<?> create(@RequestBody Category input) {
		checkPermission();
		return new ResponseEntity<>(categoryService.save1(input), HttpStatus.OK);
	}

	@GetMapping(value = "/active/{id}")
	public ResponseEntity<?> active(@PathVariable Long id) {
		checkPermission();
		Category data = categoryService.findByClientIdAndId(getClientId(), id);
		if (data == null || data.getActive()) {
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}
		data.setActive(true);
		categoryService.save(data);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@GetMapping("/deactive/{id}")
	public ResponseEntity<Category> deactive(@PathVariable Long id) {
		checkPermission();
		Category data = categoryService.findByClientIdAndId(getClientId(), id);
		if (data == null || !data.getActive()) {
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}
		try {
			categoryService.delete(data);
			data.setActive(false);
			categoryService.save(data);
			return new ResponseEntity<>(HttpStatus.OK);
		} catch (Exception e) {
			throw new RestExceptionHandler("Danh mục này hiện đang được sử dụng");
		}
	}
}
