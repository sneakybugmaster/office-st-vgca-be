package com.vz.backend.business.repository;

import com.vz.backend.business.domain.ScheduleRemindIgnore;
import com.vz.backend.core.repository.IRepository;

public interface IScheduleRemindIgnoreRepository extends IRepository<ScheduleRemindIgnore> {

	ScheduleRemindIgnore findByUserIdAndRemindId(Long userId, Long id);

}
