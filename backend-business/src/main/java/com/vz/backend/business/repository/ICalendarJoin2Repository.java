package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.CalendarJoin2;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ICalendarJoin2Repository extends IRepository<CalendarJoin2> {
	List<CalendarJoin2> findByCalendarIdAndClientIdAndActive(Long calenderId, Long clientId, boolean active);

	List<CalendarJoin2> findByCalendarIdAndClientId(Long calenderId, Long clientId); 

	@Query("SELECT COUNT(1)>0 FROM CalendarJoin2 c WHERE c.active = TRUE AND c.clientId=:clientId AND c.calendarId=:calId "
			+ " AND (c.calendar.isMeetingCalendar IS TRUE OR c.userId=:userId OR c.calendar.createBy = :userId) "
			)
	boolean isMemberOfCalendar(Long userId, Long calId, Long clientId);
}
