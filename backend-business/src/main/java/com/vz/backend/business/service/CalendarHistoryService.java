package com.vz.backend.business.service;

import java.util.List;

import com.vz.backend.core.dto.OrgGroupDto;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.business.domain.CalendarHistory;
import com.vz.backend.business.repository.ICalendarHistoryRepository;
import com.vz.backend.core.config.CalendarActionEnum;

@Service
public class CalendarHistoryService {

    @Autowired
    ICalendarHistoryRepository calendarHistoryRepo;

    public CalendarHistory save(Boolean isRegisterBan, Calendar2 c, CalendarActionEnum action) {
        CalendarHistory ch = CalendarHistory.set(c, action);
        ch.setParticipants(c.getParticipants());
        ch.setParticipantsGuest(c.getParticipantsGuest());
        if (c.getParticipantsGroup() != null) {
            String string = "";
            for (OrgGroupDto dto : c.getParticipantsGroup()) {
                string += dto.getFullName() + ", ";
            }
            ch.setParticipantsGroup(string);
        }
        if (c.getParticipantsOrg() != null) {
            String string = "";
            for (OrgGroupDto dto : c.getParticipantsOrg()) {
                string += dto.getFullName() + ", ";
            }
            ch.setParticipantsOrg(string);
        }
        if(isRegisterBan!=null) ch.setRegisterBan(isRegisterBan);
        return calendarHistoryRepo.save(ch);
    }

    public List<CalendarHistory> getByCalendarId(Long cId) {
        return calendarHistoryRepo.findByCalendarIdOrderByDateCreateDesc(cId);
    }
}
