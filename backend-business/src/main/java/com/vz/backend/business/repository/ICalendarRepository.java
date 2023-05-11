package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.Calendar;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ICalendarRepository extends IRepository<Calendar> {

	@Query("SELECT c FROM Calendar c WHERE ((EXTRACT(month FROM c.endTime) = :month AND EXTRACT(year FROM c.endTime) = :year) "
			+ " OR (EXTRACT(month FROM c.startTime) = :month " + " AND EXTRACT(year FROM c.startTime) = :year)) ")
	List<Calendar> findByMonth(int month, int year);

	List<Calendar> findByBookByAndStatus(Long bookBy, Integer status);

	@Query(value = "SELECT c FROM Calendar c WHERE c.title LIKE %:title% and c.bookBy = :bookBy and c.status = :status")
	List<Calendar> findByName(Long bookBy, Integer status, String title);
}
