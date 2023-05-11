package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.AttachmentCalendar;
import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.business.domain.CalendarComment;
import com.vz.backend.business.repository.ICalendar2Repository;
import com.vz.backend.business.repository.ICalendarCommentRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ObjTypeEnum;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class CalendarCommentService extends BaseService<CalendarComment> {

	@Autowired
	ICalendarCommentRepository cmtRepository;
	
	@Autowired
	ICalendar2Repository calendarRepository;
	
	@Autowired
	AttachmentCalendarService attService; 
	
	@Override
	public IRepository<CalendarComment> getRepository() {
		return null;
	}

	public CalendarComment getById(Long objId) {
		return cmtRepository.findByIdAndClientIdAndActiveTrue(objId, BussinessCommon.getClientId());
	}

	public CalendarComment saveCmt(CalendarComment cmt) {
		Calendar2 c = calendarRepository.findByIdAndClientIdAndActive(cmt.getCalendarId(), BussinessCommon.getClientId(), true);
		if(c == null) throw new RestExceptionHandler(Message.CALENDAR_INVALD);
		return cmtRepository.save(cmt);
	}

	public List<CalendarComment> getByCalendar(Long calendarId) {
		List<CalendarComment> cals = cmtRepository.findByCalendarIdAndClientIdAndActiveTrueOrderByIdDesc(calendarId, BussinessCommon.getClientId());
		List<Long> idCmts = cals.stream().map(CalendarComment::getId).collect(Collectors.toList());
		List<AttachmentCalendar> atts = attService.getByObjIds(idCmts, ObjTypeEnum.CALENDAR_CMT);
		if(BussinessCommon.isEmptyList(atts)) return cals;
		for (CalendarComment i : cals) {
			List<AttachmentCalendar> sub = new ArrayList<>();
			for (AttachmentCalendar a : atts) {
				if(a.getObjId().equals(i.getId())) {
					sub.add(a);
				}
			}
			i.setAttachments(sub);
		}
		return cals;
	}
}
