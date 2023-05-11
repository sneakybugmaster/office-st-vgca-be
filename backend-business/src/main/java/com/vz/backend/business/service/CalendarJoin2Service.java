package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.business.domain.CalendarJoin2;
import com.vz.backend.business.repository.ICalendarJoin2Repository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.UserService;
import org.springframework.transaction.annotation.Transactional;

@Service
public class CalendarJoin2Service extends BaseService<CalendarJoin2> {

	@Override
	public IRepository<CalendarJoin2> getRepository() {
		return calendarJoin2Repository;
	}

	@Autowired
	UserService userService;

	@Autowired
	ICalendarJoin2Repository calendarJoin2Repository;

	@Autowired
	private NotificationService notificationService;

	public List<CalendarJoin2> addCalendarJoin(Long calendarId, List<CalendarJoin2> calendarJoin) {
		List<User> userList = userService.findByClientIdAndActive(BussinessCommon.getClientId(), true);
		List<CalendarJoin2> all = new ArrayList<>();
		calendarJoin.forEach(i -> {
			if (Boolean.TRUE.equals(i.getActive())) {
				i.validCalendarJoin();
				i.setCalendarId(calendarId);
				userService.validUs(i.getUserId(), userList);
				all.add(i);
			}
		});
		return calendarJoin2Repository.saveAll(all);
	}

	public List<CalendarJoin2> getCalendarJoin(Long calenderId) {
		return calendarJoin2Repository.findByCalendarIdAndClientIdAndActive(calenderId, BussinessCommon.getClientId(), true);
	}
	
	public List<CalendarJoin2> getCalendarJoinAll(Long calenderId) {
		return calendarJoin2Repository.findByCalendarIdAndClientId(calenderId, BussinessCommon.getClientId());
	}
	
	@Transactional
	public List<CalendarJoin2> updateCalendarJoin3(Calendar2 c, List<CalendarJoin2> nList) {
		List<CalendarJoin2> rsList = new ArrayList<>();
		List<CalendarJoin2> oList = getCalendarJoinAll(c.getId());
		nList.forEach(i -> i.setCalendarId(c.getId()));
		
		List<CalendarJoin2> all = new ArrayList<>();
		all.addAll(oList);
		all.addAll(nList);
		
		all.forEach(a -> {
			if (!oList.contains(a) && nList.contains(a)) {
				// add
				rsList.add(a);
				
				notificationService.add(a.getUserId(), c.getId(), "", DocumentTypeEnum.TAO_LICH,
						NotificationHandleStatusEnum.NULL,  c.isMeetingCalendar() ? ModuleCodeEnum.CAL_MEETING : ModuleCodeEnum.CAL_BUSINESS);
				
			} else if (oList.contains(a) && !nList.contains(a)) {
				// del
				a.setActive(false);
				rsList.add(a);
				
				notificationService.setActiveByUserIdAndDocIdAndDocType(a.getUserId(), c.getId(),
						DocumentTypeEnum.TAO_LICH, false);
			}
		});
		
		return calendarJoin2Repository.saveAll(rsList);
	}

	/**
	 * userId is invited of calId
	 * @param id
	 * @return
	 */
	public boolean isMemberOfCalendar(Long userId, Long calId) {
		return calendarJoin2Repository.isMemberOfCalendar(userId, calId, BussinessCommon.getClientId());
	}
}
