package com.vz.backend.business.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.CalendarJoin;
import com.vz.backend.business.repository.ICalendarJoinRepository;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class CalendarJoinService extends BaseService<CalendarJoin> {

	@Autowired
	ICalendarJoinRepository calendarJoinRepositoty;

	@Override
	public IRepository<CalendarJoin> getRepository() {
		// TODO Auto-generated method stub
		return calendarJoinRepositoty;
	}

	public Iterable<CalendarJoin> getByCalendarId(Long calendarId) {
		return calendarJoinRepositoty.findByCalendarIdAndActive(calendarId, true);
	}

	public Iterable<CalendarJoin> addListcalendarJoin(Iterable<CalendarJoin> calendarJoin) {
		calendarJoin = calendarJoinRepositoty.saveAll(calendarJoin);
		return calendarJoin;
	}

	public void deleteListCalendarJoin(Iterable<Long> listIdCalendarJoin) {
		for (Long long1 : listIdCalendarJoin) {
			calendarJoinRepositoty.deleteById(long1);
		}
	}

}
