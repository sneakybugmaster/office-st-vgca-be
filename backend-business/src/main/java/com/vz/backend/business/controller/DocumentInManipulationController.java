package com.vz.backend.business.controller;

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
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.DocumentInManipulation;
import com.vz.backend.business.dto.DocumentInConditionDto;
import com.vz.backend.business.service.DocumentInManipulationService;
import com.vz.backend.business.service.DocumentInProcessService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;

@RestController
@RequestMapping("/doc_in_manipulation")
public class DocumentInManipulationController {
	
	@Autowired
	DocumentInManipulationService manipulationService;
	
	@Autowired
	DocumentInProcessService processService;
	
	enum SortBy {
		UPDATEDATE("updateDate"), // ngày cập nhật
		CREATEDATE("createDate"), // ngày tạo
		NUMBERSIGN("doc.numberOrSign"), // số-kí hiệu
		NUMBER_ARRIVAL("doc.numberArrival"), // số đến
		PREVIEW("doc.preview"), // trích yếu
		DOCTYPE("doc.docType.name"), // loại văn bản
		STATUS("doc.status"), // trạng thái văn bản
		SECURITY_NAME("doc.security.name"),
		FROM_USER("frUsers.fullName"),
		TO_USER("toUsers.fullName");

		private String field;

		private SortBy(String field) {
			this.field = field;
		}

		public static String getEnum(String name) {
			for (SortBy v : values()) {
				if (v.name().equals(name)) {
					return v.field;
				}
			}
			return SortBy.CREATEDATE.field;
		}
	}
	
	@GetMapping("/get")
	public ResponseEntity<Page<DocumentInManipulation>> getByDocId(
			@RequestParam(value = "type") String type,
			@RequestParam(value = "text", required = false) String text,
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		text = BussinessCommon.convert(text);
		return new ResponseEntity<>(manipulationService.findByUserId(text, type, pageable), HttpStatus.OK);
	}
	
	/**
	 * check current user has condition yet ?
	 * list condition may be has more later-> use dto, just push param in to dto
	 * @param docId
	 * @return
	 */
	@GetMapping("/checkCondition/{docId}")
	public ResponseEntity<DocumentInConditionDto> checkCondition(@PathVariable Long docId) {
		return new ResponseEntity<>(processService.checkCondition(docId, BussinessCommon.getUserId()), HttpStatus.OK);
	}
}
