package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.CalendarHistory;

@Repository
public interface ICalendarHistoryRepository extends JpaRepository<CalendarHistory, Long> {

	List<CalendarHistory> findByCalendarIdOrderByDateCreateDesc(Long cId);

}
