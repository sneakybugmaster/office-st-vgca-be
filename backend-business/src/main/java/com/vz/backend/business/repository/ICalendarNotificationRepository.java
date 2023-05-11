package com.vz.backend.business.repository;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.CalendarNotification;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface ICalendarNotificationRepository extends IRepository<CalendarNotification> {

}
