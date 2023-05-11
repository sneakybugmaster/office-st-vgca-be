package com.vz.backend.business.controller;

import java.util.Calendar;
import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.jpa.domain.JpaSort;
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

import com.vz.backend.business.controller.DocumentOutController.SortBy;
import com.vz.backend.business.domain.DocumentOutProcess;
import com.vz.backend.business.dto.DocumentProcessDto;
import com.vz.backend.business.service.DocumentOutProcessService;
import com.vz.backend.business.service.DocumentUserService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;

import lombok.Getter;

@RestController
@RequestMapping("/doc_out_process")
public class DocumentOutProcessController {

	@Getter
	enum DocProcessSortEnum {
		UPDATE_DATE("updateDate"), NUMBER_SIGN("documentOut.numberOrSign"), DOC_TYPE("documentOut.docType.name"),
		PREVIEW("documentOut.preview"), USER_ENTER("documentOut.userEnter.fullName"),
		CREATEDATE("documentOut.createDate"), HANDLE_USER("handler.fullName"), HANDLE_DATE("updateDate"),
		SECURITY("documentOut.security.name"), STATUS("handleStatus"), DELEGATE_USER("delegateUser.fullName"),
		DELEGATE("delegate.fromUser.fullName"),
		IMPORTANT("(CASE WHEN du.important IS NULL THEN 3 WHEN du.important is FALSE THEN 2 ELSE 1 END)"),
		ACTIVE("du.active"), CREATE_DATE("documentOut.createDate"),;

		private String field;

		private DocProcessSortEnum(String field) {
			this.field = field;
		}
	}

	@Autowired
	DocumentOutProcessService docOutProcessService;

	@Autowired
	DocumentUserService docUserService;

	public static DocumentOutHandleStatusEnum[] getStatus(String action) {
		switch (action) {
		case "waiting-comment":
			return new DocumentOutHandleStatusEnum[] { DocumentOutHandleStatusEnum.CHO_Y_KIEN };
		case "waiting-handle":
			return new DocumentOutHandleStatusEnum[] { DocumentOutHandleStatusEnum.BI_TRA_LAI,
					DocumentOutHandleStatusEnum.CHO_XU_LY, DocumentOutHandleStatusEnum.CHO_Y_KIEN };
		case "delegate-handled":
			return new DocumentOutHandleStatusEnum[] { DocumentOutHandleStatusEnum.DA_XU_LY_UQ, DocumentOutHandleStatusEnum.DA_TRA_LAI_UQ };
		case "issued":
		case "handled":
			return new DocumentOutHandleStatusEnum[] { DocumentOutHandleStatusEnum.DA_XU_LY,
					DocumentOutHandleStatusEnum.DA_Y_KIEN, DocumentOutHandleStatusEnum.DA_TRA_LAI };
		case "request-signe":
		case "issued2": //menu trinh ki
			return null;
//		case "all": // turn on for test
//			return null;
		default:
			throw new RestExceptionHandler("Can't find this action");
		}
	}
	
	public static DocumentStatusEnum[] getDocStatus(String action) {
		switch (action) {
		case "waiting-comment":
		case "waiting-handle":
			return new DocumentStatusEnum[] { DocumentStatusEnum.DU_THAO, DocumentStatusEnum.BI_TRA_LAI,
					DocumentStatusEnum.THU_HOI_XL, DocumentStatusEnum.DANG_XU_LY, DocumentStatusEnum.CHO_BAN_HANH, DocumentStatusEnum.DA_BAN_HANH  };
		case "delegate-handled":
		case "issued": //menu xu ly
			return new DocumentStatusEnum[] { DocumentStatusEnum.DU_THAO, DocumentStatusEnum.BI_TRA_LAI,
					DocumentStatusEnum.THU_HOI_XL, DocumentStatusEnum.DANG_XU_LY, DocumentStatusEnum.CHO_BAN_HANH };
		case "handled":
			return new DocumentStatusEnum[] { DocumentStatusEnum.DA_BAN_HANH };
		case "request-signe":
			return new DocumentStatusEnum[] { DocumentStatusEnum.CHO_BAN_HANH, DocumentStatusEnum.DANG_XU_LY };
		case "issued2": //menu trinh ki
			return new DocumentStatusEnum[] { DocumentStatusEnum.DA_BAN_HANH };
		default:
			throw new RestExceptionHandler("Can't find this action");
		}
	}
	

	@PostMapping(value = "/add")
	public ResponseEntity<DocumentOutProcess> add(@RequestBody DocumentOutProcess input) {
		input.setHandleStatus(DocumentOutHandleStatusEnum.DU_THAO);
		docOutProcessService.validSaveDocOutProcess(input);
		return new ResponseEntity<>(docOutProcessService.save(input), HttpStatus.OK);
	}

	@PostMapping(value = "/setImportant")
	public ResponseEntity<?> setImportant(@RequestParam Long docId, @RequestParam Boolean important) {
		return new ResponseEntity<>(docUserService.setImportant(DocumentTypeEnum.VAN_BAN_DI, docId, important),
				HttpStatus.OK);
	}

	// for test
	@GetMapping("/getById/{id}")
	public ResponseEntity<DocumentOutProcess> getById(@PathVariable Long id) {
		DocumentOutProcess dop = docOutProcessService.findByClientIdAndId(BussinessCommon.getClientId(), id);
		return new ResponseEntity<>(dop, HttpStatus.OK);
	}

	@GetMapping("/handling/{action}")
	public ResponseEntity<Page<DocumentProcessDto>> handling(
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) DocProcessSortEnum sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size, @PathVariable("action") String action,
			@RequestParam(defaultValue = "1") int page) {
		DocumentOutHandleStatusEnum[] handleStatus = DocumentOutProcessController.getStatus(action);
		DocumentStatusEnum[] docStatus =  DocumentOutProcessController.getDocStatus(action);
		Sort sort = JpaSort.unsafe(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		Page<DocumentProcessDto> dopList = docOutProcessService.findByHandleStatus(handleStatus, docStatus, action , pageable);
		return new ResponseEntity<>(dopList, HttpStatus.OK);
	}

	@GetMapping("/handling/{action}/search")
	public ResponseEntity<Page<DocumentProcessDto>> search(
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) DocProcessSortEnum sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size, @PathVariable("action") String action,
			@RequestParam(name = "q", defaultValue = "") String text, @RequestParam(defaultValue = "1") int page) {
		DocumentOutHandleStatusEnum[] handleStatus = DocumentOutProcessController.getStatus(action);
		DocumentStatusEnum[] docStatus =  DocumentOutProcessController.getDocStatus(action);
		text = StringUtils.handleSubmit(text);
		if (text == null) {
			return handling(sortBy, direction, size, action, page);
		}
		Sort sort = JpaSort.unsafe(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		Page<DocumentProcessDto> dopList = docOutProcessService.search(handleStatus, docStatus, text, pageable);
		return new ResponseEntity<>(dopList, HttpStatus.OK);
	}

	@GetMapping("/handling/{action}/search-advance")
	public ResponseEntity<Page<DocumentProcessDto>> searchAdvance(
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) DocProcessSortEnum sortBy,
			@RequestParam(defaultValue = "DESC") Direction direction, @RequestParam(required = false) Boolean important,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size, @PathVariable("action") String action,
			@RequestParam(name = "preview", required = false) String preview,
			@RequestParam(name = "sign", required = false) String numberOrSign,
			@RequestParam(name = "docTypeId", required = false) Long docTypeId,
			@RequestParam(name = "docFieldId", required = false) Long docFieldId,
			@RequestParam(name = "orgName", required = false) String orgCreateName,
			@RequestParam(name = "userEnter", required = false) String userEnter,
			@RequestParam(name = "startDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@RequestParam(name = "endDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date endDate,
			@RequestParam(defaultValue = "1") int page) {
		DocumentOutHandleStatusEnum[] handleStatus = DocumentOutProcessController.getStatus(action);
		DocumentStatusEnum[] docStatus = DocumentOutProcessController.getDocStatus(action);
		preview = StringUtils.handleSubmit(preview);
		numberOrSign = StringUtils.handleSubmit(numberOrSign);
		orgCreateName = StringUtils.handleSubmit(orgCreateName);
		userEnter = StringUtils.handleSubmit(userEnter);
		startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, -1);
		endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1);
		Sort sort;
		Boolean sortByImportant = false;
		if (DocProcessSortEnum.UPDATE_DATE.equals(sortBy)) {
			sort = Sort.by(direction, sortBy.field);
			sortByImportant = true;
		} else {
			sort = Sort.by(direction, sortBy.field);
		}
		Pageable pageable = PageRequest.of(page - 1, size, sort);

		Page<DocumentProcessDto> dopList = docOutProcessService.searchAdvance(important, handleStatus, docStatus, preview, numberOrSign,
				docTypeId, docFieldId, orgCreateName, userEnter, startDate, endDate, sortByImportant, pageable);
		return new ResponseEntity<>(dopList, HttpStatus.OK);
	}
}
