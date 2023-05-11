package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.EntityManagerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.Calendar;
import com.vz.backend.business.repository.ICalendarRepository;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class CalendarService extends BaseService<Calendar> {

	@Autowired
	ICalendarRepository calendarRepository;

	@Autowired
	EntityManagerFactory entityManagerFactory;

	@Override
	public IRepository<Calendar> getRepository() {
		return calendarRepository;
	}

	public List<Calendar> findByMonth(int month, int year) {
		List<Calendar> calendar = calendarRepository.findByMonth(month, year);
		return calendar;
	}

	public Calendar addCalendar(Calendar calendar) {
		calendar = calendarRepository.save(calendar);
		return calendar;
	}

	public void updateCalendar(Calendar calendar, Long id) {
		calendar.setId(id);
		calendarRepository.save(calendar);
	}

	public List<Calendar> findByBookBy(Long bookBy, Integer status) {
		List<Calendar> calendars = new ArrayList<>();
		if (status > 1) {
			List<Calendar> cld = calendarRepository.findByBookByAndStatus(bookBy, 2);
			calendars = calendarRepository.findByBookByAndStatus(bookBy, 3);
			for (Calendar calendar : cld) {
				calendars.add(calendar);
			}
		} else {
			calendars = calendarRepository.findByBookByAndStatus(bookBy, status);
		}
		return calendars;
	}

	public List<Calendar> findByTile(Long bookBy, Integer status, String title) {
		List<Calendar> calendars;
		if (status > 1) {
			List<Calendar> cld = calendarRepository.findByName(bookBy, 2, title);
			calendars = calendarRepository.findByName(bookBy, 3, title);
			for (Calendar calendar : cld) {
				calendars.add(calendar);
			}
		} else {
			calendars = calendarRepository.findByName(bookBy, status, title);
		}
		return calendars;
	}
}
