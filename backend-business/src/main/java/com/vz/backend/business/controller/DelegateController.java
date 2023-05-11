package com.vz.backend.business.controller;

import java.util.Calendar;
import java.util.Date;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.DateTimeFormat.ISO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.google.common.net.HttpHeaders;
import com.vz.backend.business.controller.DocumentOutProcessController.DocProcessSortEnum;
import com.vz.backend.business.domain.AttachmentDelegate;
import com.vz.backend.business.domain.Delegate;
import com.vz.backend.business.dto.DelegateDto;
import com.vz.backend.business.dto.DocumentProcessDto;
import com.vz.backend.business.service.DelegateService;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestFieldExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.core.service.RoleService;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/delegate")
public class DelegateController {
	enum SortBy {
		UPDATE_DATE("updateDate"), FROM_USER_ORDER("fromUser.positionModel.order"),
		TO_USER_ORDER("toUser.positionModel.order"), NUMBER_SIGN("numberOrSign"), START_DATE("startDate"),
		END_DATE("endDate");

		private String field;

		private SortBy(String field) {
			this.field = field;
		}
	}

	@Autowired
	DelegateService delegateService;

	@Autowired
	private RoleService roleService;

	@Autowired
	FilesStorageService storageService;

	private DocumentOutHandleStatusEnum[] status = DocumentOutProcessController.getStatus("waiting-handle");

	@PostMapping("/add")
	public ResponseEntity<Delegate> add(@RequestParam Long fromUserId, @RequestParam Long toUserId,
			@RequestParam String numberOrSign, @RequestParam @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@RequestParam @DateTimeFormat(iso = ISO.DATE) Date endDate,
			@RequestParam(required = false) MultipartFile[] files) {
		requirePermission();

		if (endDate.compareTo(startDate) < 0) {
			throw new RestFieldExceptionHandler("endDate", "Ngày hết hạn không thể trước ngày bắt đầu");
		}
		if (fromUserId.compareTo(toUserId) == 0) {
			throw new RestFieldExceptionHandler("toUserId", "Bạn phải uỷ quyền cho người khác");
		}
		numberOrSign = numberOrSign.trim();
		startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, 0);
		endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1, -1);

		if (new Date().compareTo(endDate) > 0) {
			throw new RestFieldExceptionHandler("endDate", "Đã quá ngày hết hạn");
		}
		Delegate delegate = delegateService.add(fromUserId, toUserId, numberOrSign, startDate, endDate, files);
		return new ResponseEntity<>(delegate, HttpStatus.OK);
	}

	@GetMapping("/getDetailById")
	public ResponseEntity<?> getDetailById(@RequestParam Long id) {
		requirePermission();
		return new ResponseEntity<>(delegateService.getDetailById(id), HttpStatus.OK);
	}

	@GetMapping("/activeAndDeactive")
	public ResponseEntity<?> activeAndDeactive(@RequestParam Long id) {
		requirePermission();
		return new ResponseEntity<>(delegateService.activeAndDeactive(id), HttpStatus.OK);
	}

	@PostMapping("/update")
	public ResponseEntity<Delegate> update(@RequestParam Long delegateId,
			@RequestParam(required = false) String numberOrSign,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date endDate,
			@RequestParam(required = false) MultipartFile[] files) {
		requirePermission();
		numberOrSign = StringUtils.handleSubmit(numberOrSign);
		startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, 0);
		endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1, -1);

		Delegate delegate = delegateService.update(delegateId, numberOrSign, startDate, endDate,
				files);
		return new ResponseEntity<>(delegate, HttpStatus.OK);
	}

	@GetMapping("/list")
	public ResponseEntity<Page<DelegateDto>> list(@RequestParam(defaultValue = "1") int page,
												  @RequestParam(defaultValue = "UPDATE_DATE") SortBy sortBy,
												  @RequestParam(defaultValue = "DESC") Direction direction,
												  @RequestParam(defaultValue = "true") Boolean isShowAll,
												  @RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size) {
		requirePermission();

		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(delegateService.list(pageable, isShowAll), HttpStatus.OK);
	}

	@GetMapping("/quick-search")
	public ResponseEntity<Page<DelegateDto>> quickSearch(@RequestParam(required = false) String q,
			@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "UPDATE_DATE") SortBy sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size) {

		q = StringUtils.handleSubmit(q);
		if (q == null) {
			return list(page, sortBy, direction, true, size);
		}
		requirePermission();

		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(delegateService.quickSearch(q, pageable), HttpStatus.OK);
	}

	@GetMapping("/search")
	public ResponseEntity<Page<DelegateDto>> search(@RequestParam(required = false) String numberOrSign,
			@RequestParam(required = false) String fromUser, @RequestParam(required = false) String toUser,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date endDate,
			@RequestParam(defaultValue = "1") int page, @RequestParam(defaultValue = "UPDATE_DATE") SortBy sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size) {

		numberOrSign = StringUtils.handleSubmit(numberOrSign);
		fromUser = StringUtils.handleSubmit(fromUser);
		toUser = StringUtils.handleSubmit(toUser);
		startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, -1);
		endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1);
		requirePermission();

		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(
				delegateService.search(numberOrSign, fromUser, toUser, startDate, endDate, pageable), HttpStatus.OK);
	}

	@GetMapping("/handling")
	public ResponseEntity<Page<DocumentProcessDto>> handling(
			@RequestParam(defaultValue = "UPDATE_DATE") DocProcessSortEnum sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = "1") int page) {
		Sort sort = Sort.by(direction, sortBy.getField());
		Pageable pageable = PageRequest.of(page - 1, size, sort);

		Page<DocumentProcessDto> dopList = delegateService.findByHandleStatus(status, pageable);
		return new ResponseEntity<>(dopList, HttpStatus.OK);
	}

	@GetMapping("/handling/search")
	public ResponseEntity<Page<DocumentProcessDto>> search(
			@RequestParam(defaultValue = "UPDATE_DATE") DocProcessSortEnum sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(name = "q", defaultValue = "") String text, @RequestParam(defaultValue = "1") int page) {

		text = StringUtils.handleSubmit(text);
		if (text == null) {
			return handling(sortBy, direction, size, page);
		}

		Sort sort = Sort.by(direction, sortBy.getField());
		Pageable pageable = PageRequest.of(page - 1, size, sort);

		Page<DocumentProcessDto> dopList = delegateService.search(status, text, pageable);
		return new ResponseEntity<>(dopList, HttpStatus.OK);
	}

	@GetMapping("/handling/search-advance")
	public ResponseEntity<Page<DocumentProcessDto>> searchAdvance(
			@RequestParam(defaultValue = "UPDATE_DATE") DocProcessSortEnum sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(required = false) Boolean important,
			@RequestParam(name = "preview", required = false) String preview,
			@RequestParam(name = "sign", required = false) String numberOrSign,
			@RequestParam(name = "docTypeId", required = false) Long docTypeId,
			@RequestParam(name = "docFieldId", required = false) Long docFieldId,
			@RequestParam(name = "orgName", required = false) String orgCreateName,
			@RequestParam(name = "userEnter", required = false) String userEnter,
			@RequestParam(name = "startDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@RequestParam(name = "endDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date endDate,
			@RequestParam(defaultValue = "1") int page) {

		preview = StringUtils.handleSubmit(preview);
		numberOrSign = StringUtils.handleSubmit(numberOrSign);
		orgCreateName = StringUtils.handleSubmit(orgCreateName);
		userEnter = StringUtils.handleSubmit(userEnter);
		startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, -1);
		endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1);

		Sort sort = Sort.by(direction, sortBy.getField());
		Pageable pageable = PageRequest.of(page - 1, size, sort);

		Page<DocumentProcessDto> dopList = delegateService.searchAdvance(important, status, preview, numberOrSign,
				docTypeId, docFieldId, orgCreateName, userEnter, startDate, endDate, pageable);
		return new ResponseEntity<>(dopList, HttpStatus.OK);
	}

	private void requirePermission() {
		if (roleService.existUserInModule(ModuleCodeEnum.DELEGATE_MANAGE.getName())) {
			return;
		}
		throw new RestForbidden("Bạn không có quyền truy cập vào uỷ quyền");
	}

	@GetMapping("/download/{fileName:.+}")
	@ResponseBody
	public ResponseEntity<Resource> getFile(@PathVariable String fileName) {
		Optional<AttachmentDelegate> attach = delegateService.findByFileName(fileName);
		if (!attach.isPresent()) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}

		Resource file = storageService.load(StringUtils.decodeFromUrl(fileName));
		if (file == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + file.getFilename() + "\"")
				.body(file);
	}
}
