package com.vz.backend.business.controller;

import java.util.Calendar;
import java.util.Date;

import org.springframework.beans.factory.annotation.Autowired;
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
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.domain.WordEditor;
import com.vz.backend.business.service.WordEditorService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.util.DateTimeUtils;

@RestController
@RequestMapping("/we")
public class WordEditorController {
	
	enum SortBy {
		UPDATEDATE("updateDate"), // Ngày cập nhật
		CREATEDATE("createDate"), // Ngày tạo
		NAME("name"), // tên công việc
		START_DATE("startDate"), // Ngày bắt đầu
		END_DATE("endDate"), // ngày kết thúc
		STATUS("status"), // trạng thái văn bản soạn thảo
		HANDLE_STATUS("p.handleStatus"), // trạng thái xử lý
		CATEGORY("cat.name"), // lĩnh vực
		;
		private String field;

		private SortBy(String field) {
			this.field = field;
		}
	}
	
	@Autowired
	WordEditorService wordEditorService;
	
	@PostMapping(value = "/transfer/{weId}")
	public ResponseEntity<?> transfer(@PathVariable Long weId, @RequestParam(value = "comment") String comment,
			@RequestParam() Long[] main,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date deadline,
			@RequestParam(value = "node") Long node, @RequestParam(value = "files", required = false) MultipartFile[] files) {
		deadline = deadline == null ? null : DateTimeUtils.handleSubmit(deadline);
		return new ResponseEntity<>(wordEditorService.transfer(weId, main, node, comment, files), HttpStatus.OK);
	}
	
	@PostMapping(value = "/add")
	public ResponseEntity<?> add(@RequestBody WordEditor wordEditor) {
		return new ResponseEntity<>(wordEditorService.add(wordEditor), HttpStatus.OK);
	}
	
	@PostMapping(value = "/done/{weId}")
	public ResponseEntity<?> done(@PathVariable Long weId, @RequestParam(value = "comment") String comment,
			@RequestParam(value = "files", required = false) MultipartFile[] files) {
		return new ResponseEntity<>(wordEditorService.done(weId, comment, files), HttpStatus.OK);
	}
	
	@GetMapping(value = "/get/{weId}")
	public ResponseEntity<?> get(@PathVariable Long weId) {
		wordEditorService.canRead(BussinessCommon.getUserId(), weId);
		return new ResponseEntity<>(wordEditorService.get(weId), HttpStatus.OK);
	}
	
	@GetMapping(value = "/get-list")
	public ResponseEntity<?> getList() {
		return new ResponseEntity<>(wordEditorService.getList(), HttpStatus.OK);
	}
	
	@GetMapping(value = "/list")
	public ResponseEntity<?> list(
			@RequestParam(defaultValue = com.vz.backend.core.config.Constant.DEFAULT_SORT_BY) SortBy sortBy,
			@RequestParam(defaultValue = com.vz.backend.core.config.Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(value = "size", defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = com.vz.backend.core.config.Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam(required = false) String text, //name/ description
			@RequestParam(required = false ) @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date endDate,
			@RequestParam(required = false) DocumentStatusEnum status
			) {
		Sort sort = JpaSort.unsafe(direction, sortBy.field);
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		text = BussinessCommon.convert(text);
		startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, -1);
		endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1);
		return new ResponseEntity<>(wordEditorService.list(text, startDate, endDate, status, pageable), HttpStatus.OK);
	}
	
	@PostMapping(value = "/del/{weId}")
	public ResponseEntity<?> del(@PathVariable Long weId) {
		return new ResponseEntity<>(wordEditorService.del(weId), HttpStatus.OK);
	}
	
	@GetMapping(value = "/tracking/{weId}")
	public ResponseEntity<?> tracking(@PathVariable Long weId) {
		return new ResponseEntity<>(wordEditorService.tracking(weId), HttpStatus.OK);
	}

	@PostMapping(value = "/comment/{weId}")
	public ResponseEntity<?> comment(@PathVariable Long weId, @RequestParam(value = "comment", required = false) String comment,
			@RequestParam(value = "files", required = false) MultipartFile[] files) {
		wordEditorService.saveCommentAndAttach(weId, files, comment);
		return new ResponseEntity<>(Boolean.TRUE, HttpStatus.OK);
	}
	
	@GetMapping(value = "/comment/{weId}")
	public ResponseEntity<?> comment(@PathVariable Long weId) {
		return new ResponseEntity<>(wordEditorService.comment(weId), HttpStatus.OK);
	}
}
