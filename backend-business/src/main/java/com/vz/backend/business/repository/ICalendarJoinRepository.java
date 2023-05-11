package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.CalendarJoin;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ICalendarJoinRepository extends IRepository<CalendarJoin> {

	List<CalendarJoin> findByCalendarIdAndActive(Long calendarid, Boolean active);

}
