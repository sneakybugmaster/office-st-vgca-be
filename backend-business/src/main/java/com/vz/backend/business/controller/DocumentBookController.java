package com.vz.backend.business.controller;

import java.util.Arrays;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
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
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.DocumentBook;
import com.vz.backend.business.dto.DocumentBookDto;
import com.vz.backend.business.dto.DocumentBookWithListOrgIds;
import com.vz.backend.business.service.CategoryDocBookService;
import com.vz.backend.business.service.DocumentBookService;
import com.vz.backend.business.service.OrgDocBookService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.service.IService;
import com.vz.backend.core.service.UserService;
import com.vz.backend.util.StringUtils;

/**
 * @author DucND
 * @date May 29, 2020
 */
@RestController
@RequestMapping("/document-book")
public class DocumentBookController {
	@Value("${configs.doc-book.org_config: false}")
	private boolean orgConfig;

	enum SortBy {
		UPDATE_DATE("updateDate"), NAME("name"), ACTIVE("active"), NUMBER_SIGN("numberOrSign"),
		CURRENT_NUMBER("currentNumber"), BOOK_TYPE("bookType"), YEAR("year");

		private String field;

		private SortBy(String field) {
			this.field = field;
		}
	}

	@Autowired
	private DocumentBookService documentBookService;

	@Autowired
	private OrgDocBookService orgDocBookService;
	
	@Autowired
	private UserService userService;
	
	@Autowired 
	private CategoryDocBookService categoryDocBookService;

	public IService<DocumentBook> getService() {
		return documentBookService;
	}

	private void checkPermission() {
		if (userService.checkUserIdByModuleCodeAndClientId(BussinessCommon.getUserId(),
				Arrays.asList(ModuleCodeEnum.DOCUMENT_BOOK.getName()), BussinessCommon.getClientId()))
			return;
		throw new RestForbidden("Bạn không có quyền truy cập vào sổ văn bản");
	}

	@PostMapping("/add")
	public ResponseEntity<?> add(@RequestBody DocumentBookWithListOrgIds input) {
		checkPermission();
		return new ResponseEntity<>(documentBookService.add(input), HttpStatus.OK);
	}

	@PostMapping("/update/{id}")
	public ResponseEntity<?> update(@PathVariable Long id, @RequestBody DocumentBookWithListOrgIds input) {
		checkPermission();
		return new ResponseEntity<>(documentBookService.update(id, input), HttpStatus.OK);
	}
	
	@GetMapping("/getDetailById/{id}")
	public ResponseEntity<?> getDetailById(@PathVariable Long id) {
		checkPermission();
		return new ResponseEntity<>(documentBookService.getDetailById(id), HttpStatus.OK);
	}

	@GetMapping("/getAllPaging")
	public ResponseEntity<Page<DocumentBookDto>> getAllPaging(@RequestParam(defaultValue = "1") int page,
			@RequestParam(defaultValue = "UPDATE_DATE") SortBy sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size) {
		checkPermission();
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		Page<DocumentBookDto> data = documentBookService.findPageByOrgIdAndClientId(pageable);
		return new ResponseEntity<>(data, HttpStatus.OK);
	}

	@PostMapping(value = "/searchDocumentBook")
	public ResponseEntity<Page<DocumentBookDto>> searchDocumentBook(@RequestParam(defaultValue = "1") int page,
			@RequestParam(defaultValue = "UPDATE_DATE") SortBy sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(required = false) String name, @RequestParam(required = false) Long type,
			@RequestParam(required = false) Integer year, @RequestParam(required = false) Boolean status) {
		checkPermission();
		if (StringUtils.isNullOrEmpty(name)) {
			name = null;
		}
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(documentBookService.searchDocumentBook(name, type, year, status, pageable),
				HttpStatus.OK);
	}

	@GetMapping(value = "/getByType/{typeCode}")
	public ResponseEntity<?> getByType(@PathVariable Long typeCode) {
		return new ResponseEntity<>(documentBookService.findByBookType(typeCode), HttpStatus.OK);
	}

	@GetMapping(value = "/getByTypeFlowing/{typeCode}")
	public ResponseEntity<?> getByTypeFlowing(@PathVariable Long typeCode) {
		return new ResponseEntity<>(documentBookService.findByBookTypeFlowing(typeCode), HttpStatus.OK);
	}


	@GetMapping(value = "/active/{id}")
	public ResponseEntity<?> active(@PathVariable Long id) {
		checkPermission();
		return updateStatus(id, true);
	}

	@GetMapping(value = "/deactive/{id}")
	public ResponseEntity<?> deactive(@PathVariable Long id) {
		checkPermission();
		return updateStatus(id, false);
	}

	public ResponseEntity<?> updateStatus(Long id, Boolean active) {
		DocumentBook item = getService().getOne(id);
		if (item != null) {
			if (orgConfig && !item.getOrgCreateId().equals(BussinessCommon.getUser().getOrg())) {
				throw new RestExceptionHandler(Message.DOCUMENT_BOOK_UPDATE);
			}
			if (!active.equals(item.getActive())) {
				item.setActive(active);
				getService().save(item);
			}
		} else {
			throw new RestExceptionHandler(Message.DOCUMENT_BOOK_NOT_FOUND);
		}
		DocumentBookDto rep = new DocumentBookDto(item.getId(), item.getActive(), item.getName(), item.getStartNumber(),
				item.getCurrentNumber(), item.getBookType(), item.getNumberOrSign(), item.getYear(),
				item.getOrg().getId(), item.getOrg().getName());
		rep.setOrgIds(orgDocBookService.findOrgIdByBookIdAndActive(rep.getId(), true));
		return new ResponseEntity<>(rep, HttpStatus.OK);
	}
}
