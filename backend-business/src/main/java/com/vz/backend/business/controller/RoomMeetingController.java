package com.vz.backend.business.controller;

import java.util.Date;
import java.util.List;

import com.vz.backend.core.config.CalendarStatusEnum;
import com.vz.backend.util.DateTimeUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.RoomMeeting;
import com.vz.backend.business.service.RoomMeetingService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;

@RestController
@RequestMapping("meeting-room")
public class RoomMeetingController {
	
	enum SortBy {
		UPDATEDATE("updateDate"), // Ngày cập nhật
		CREATEDATE("createDate"), // Ngày tạo
		NAME("name"), 
		ADDRESS("address"), 
		DESCRIPTION("description"), 
		QUANTITY("quantity"), 
		ACREAGE("acreage");

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
	
	@Autowired
	RoomMeetingService roomService;
	
	@PostMapping(value = "/addRoom")
	public ResponseEntity<?> addRoom(@RequestBody RoomMeeting room) {
		return new ResponseEntity<>(roomService.add(room), HttpStatus.OK);
	}
	
	@GetMapping("/getRoom")
	public ResponseEntity<List<RoomMeeting>> getRoom() {
		return new ResponseEntity<>(roomService.getRoom(), HttpStatus.OK);
	}
	
	@GetMapping("/getRoomPage")
	public ResponseEntity<Page<RoomMeeting>> getRoomPage(
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam(required = false, name = "q") String text,
			@RequestParam(required = false) String name,
			@RequestParam(required = false) String address,
			@RequestParam(required = false) String description,
			@RequestParam(required = false) Integer quantity,
			@RequestParam(required = false ) Float acreage
			) {
		text = BussinessCommon.convert(text);
		name = BussinessCommon.convert(name);
		address = BussinessCommon.convert(address);
		description = BussinessCommon.convert(description);
		quantity = BussinessCommon.convert(quantity);
		acreage = BussinessCommon.convert(acreage);
		Pageable pageable = BussinessCommon.castToPageable(page, Sort.by(direction, SortBy.getEnum(sortBy)), size);
		return new ResponseEntity<>(roomService.getRoomPage(text, name, address, description, quantity, acreage, pageable), HttpStatus.OK);
	}
	
	@PostMapping(value = "/delRoom/{id}")
	public ResponseEntity<RoomMeeting> delRoom(@PathVariable Long id) {
		return new ResponseEntity<>(roomService.inActive(id), HttpStatus.OK);
	}

	@GetMapping(value = "/getStatistic")
	public ResponseEntity<?> getStatistic(@RequestParam Long roomId, @RequestParam Long chairmanId, @RequestParam String name,
										  @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) Date startDate,
										  @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) Date endDate,
										  @RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
										  @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		Pageable pageable = PageRequest.of(page - 1, size);
		startDate = DateTimeUtils.handleSubmit(startDate);
		endDate = DateTimeUtils.getEndDate(endDate);
		return new ResponseEntity<>(roomService.getStatistic(startDate, endDate, roomId, chairmanId, name, CalendarStatusEnum.APPROVE, pageable), HttpStatus.OK);
	}

	@GetMapping(value = "/getAllByName")
	public ResponseEntity<?> getAllByName(@RequestParam String name) {
		return new ResponseEntity<>(roomService.getAllByName(name), HttpStatus.OK);
	}
}
