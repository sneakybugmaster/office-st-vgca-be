package com.vz.backend.business.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.CalendarComment;
import com.vz.backend.business.service.CalendarCommentService;
import com.vz.backend.core.common.BussinessCommon;

@RestController
@RequestMapping("/calendar-comment")
public class CalendarCommentController {

	@Autowired
	CalendarCommentService cmtService;
	
	@PostMapping(value = "/saveCmt")
	public ResponseEntity<?> saveCmt(@RequestBody CalendarComment cmt) {
		BussinessCommon.validLengthData(cmt.getComment(), "Ý kiến xử lý", 2000);
		return new ResponseEntity<>(cmtService.saveCmt(cmt), HttpStatus.OK);
	}
	
	@GetMapping(value = "/getByCalendar/{calendarId}")
	public ResponseEntity<?> getByCalendar(@PathVariable Long calendarId) {
		return new ResponseEntity<>(cmtService.getByCalendar(calendarId), HttpStatus.OK);
	}

}
