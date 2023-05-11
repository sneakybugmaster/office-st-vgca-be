package com.vz.backend.business.controller;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.business.domain.Calendar;
import com.vz.backend.business.domain.CalendarJoin;
import com.vz.backend.business.service.CalendarJoinService;
import com.vz.backend.business.service.CalendarService;
import com.vz.backend.business.service.RoomService;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.service.IService;

@RestController
@RequestMapping("/calendar")
public class CalendarController extends BaseController<Calendar> {

	@Autowired
	RoomService roomService;

	@Autowired
	CalendarService calendarService;

	@Autowired
	CalendarJoinService calendarJoinService;

	@Override
	public IService<Calendar> getService() {
		return calendarService;
	}

	@PostMapping("/getByMonth")
	public ResponseEntity<Iterable<Calendar>> getAllCalendar(@RequestParam("month") int month,
			@RequestParam("year") int year) {
		return new ResponseEntity<>(calendarService.findByMonth(month, year), HttpStatus.OK);
	}

	@GetMapping("/getCalendarJoin/{calendarId}")
	public ResponseEntity<Iterable<CalendarJoin>> getCalendarJoin(@PathVariable("calendarId") Long calendarId) {
		return new ResponseEntity<>(calendarJoinService.getByCalendarId(calendarId), HttpStatus.OK);
	}

	@PostMapping(value = "/addCalendar")
	public ResponseEntity<?> addCalendar(@RequestBody Calendar calendar) {
		calendar.setActive(true);
		calendar.setOrgId(this.getCurrentUser().getOrg());
		return new ResponseEntity<>(calendarService.addCalendar(calendar), HttpStatus.OK);
	}

	@PostMapping(value = "/updateCalendar/{id}")
	public ResponseEntity<?> updateCalendar(@RequestBody Calendar calendar, @PathVariable Long id) {
		calendarService.updateCalendar(calendar, id);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@PostMapping(value = "/addlistJoin/{calendarId}")
	public ResponseEntity<?> addListCanlendarJoin(@RequestBody Iterable<CalendarJoin> calendarJoins,
			@PathVariable("calendarId") Long calendarId) {
		if (calendarJoins != null) {
			for (CalendarJoin calendarJoin : calendarJoins) {
				calendarJoin.setCalendarId(calendarId);
			}
		}
		return new ResponseEntity<>(calendarJoinService.addListcalendarJoin(calendarJoins), HttpStatus.OK);
	}

	@PostMapping(value = "/deleteJoin")
	public ResponseEntity<?> deleteCalendarjoin(@RequestBody Iterable<Long> idCalendarJoin) {
		calendarJoinService.deleteListCalendarJoin(idCalendarJoin);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@PostMapping(value = "/findByBookBy")
	public ResponseEntity<List<Calendar>> findByBookBy(@RequestParam(name = "bookBy") Long bookById,
			@RequestParam(name = "status") Integer fieldStatus) {
		return new ResponseEntity<>(calendarService.findByBookBy(bookById, fieldStatus), HttpStatus.OK);
	}

	@PostMapping(value = "/findByName")
	public ResponseEntity<List<Calendar>> findByName(@RequestParam(name = "bookBy") Long bookBy,
			@RequestParam(name = "status") Integer status, @RequestParam(name = "title") String title) {
		return new ResponseEntity<>(calendarService.findByTile(bookBy, status, title), HttpStatus.OK);
	}

	@GetMapping(value = "/getTimes")
	public ResponseEntity<?> timeInCalendar() {
		// Get start time
		// Get end time
		List<String> result = new ArrayList<>();
		result.add("07:00");
		result.add("07:30");
		result.add("08:00");
		result.add("08:30");
		result.add("09:00");
		result.add("09:30");
		result.add("10:00");
		result.add("10:30");
		result.add("11:00");
		result.add("11:30");
		result.add("12:00");
		result.add("12:30");
		result.add("13:00");
		result.add("13:30");
		result.add("14:00");
		result.add("14:30");
		result.add("15:00");
		result.add("15:30");
		result.add("16:00");
		result.add("16:30");
		result.add("17:00");
		result.add("17:30");
		return new ResponseEntity<>(result, HttpStatus.OK);
	}

}
