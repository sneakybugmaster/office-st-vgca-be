package com.vz.backend.business.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.business.domain.CalendarHistory;
import com.vz.backend.business.service.CalendarHistoryService;
import com.vz.backend.core.config.CalendarActionEnum;

@RestController
@RequestMapping("/calendar_history")
public class CalendarHistoryController {
	@Autowired
	CalendarHistoryService calendarHistoryservice;
	
	@GetMapping("/getByCalendarId/{cId}")
	public ResponseEntity<List<CalendarHistory>> getByCalendarId(@PathVariable Long cId) {
		return new ResponseEntity<>(calendarHistoryservice.getByCalendarId(cId), HttpStatus.OK);
	}
	
	@PostMapping("/add")
	public ResponseEntity<CalendarHistory> add(@RequestBody Calendar2 input) {
		return new ResponseEntity<>(calendarHistoryservice.save(null, input, CalendarActionEnum.ADD), HttpStatus.OK);
	}
}
