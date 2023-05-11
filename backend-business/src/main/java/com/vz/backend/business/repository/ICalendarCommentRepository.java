package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.CalendarComment;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ICalendarCommentRepository extends IRepository<CalendarComment> {

	CalendarComment findByIdAndClientIdAndActiveTrue(Long id, Long clientId);

	List<CalendarComment> findByCalendarIdAndClientIdAndActiveTrueOrderByIdDesc(Long calendarId, Long clientId);

}
