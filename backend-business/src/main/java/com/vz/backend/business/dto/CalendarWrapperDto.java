package com.vz.backend.business.dto;

import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.domain.AttachmentCalendar;
import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.apache.commons.lang.time.DateUtils;

@AllArgsConstructor
@NoArgsConstructor
@Data
public class CalendarWrapperDto {
	private List<CalendarDto> objList ;
	private Date frDate;
	private Date toDate;
	private int week;
	private AttachmentCalendar attCalWeek;
	
	@JsonIgnore
	private Map<Integer, List<Calendar2>> calendarListByDate = setCalendarDefault();
	
	@JsonIgnore
	private List<CalendarDto> calendarListByAmPm ;
	
	private static Map<Integer, String> dateOfWeek = DateTimeUtils.dayOfWeekMap;

	public Map<Integer, List<Calendar2>> setCalendarDefault() {
		calendarListByDate = new LinkedHashMap<>();
		dateOfWeek.forEach((k,v) -> {
			calendarListByDate.put(k, new ArrayList<>());
		});
		return calendarListByDate;
	}
	
	public CalendarWrapperDto(List<Calendar2> all, Date frDate, Date toDate, int week, int year, AttachmentCalendar attCalWeek) {
		this.frDate = frDate;
		this.toDate = toDate;
		this.week = week;
		this.initObjList(all, year);
		this.attCalWeek = attCalWeek;
	}

	private void initObjList(List<Calendar2> calendar2s, int year) {
		Map<Integer, CalendarDto> calendarByDayOfWeek = new LinkedHashMap<>();
		calendarByDayOfWeek.put(Calendar.MONDAY, null);
		calendarByDayOfWeek.put(Calendar.TUESDAY, null);
		calendarByDayOfWeek.put(Calendar.WEDNESDAY, null);
		calendarByDayOfWeek.put(Calendar.THURSDAY, null);
		calendarByDayOfWeek.put(Calendar.FRIDAY, null);
		calendarByDayOfWeek.put(Calendar.SATURDAY, null);
		calendarByDayOfWeek.put(Calendar.SUNDAY, null);
		for (Calendar2 calendar2 : calendar2s) {
			this.processCalendar(calendarByDayOfWeek, calendar2);
		}

		this.objList = new ArrayList<>();
		for (Map.Entry<Integer, CalendarDto> entry : calendarByDayOfWeek.entrySet()) {
			CalendarDto calendarDto = entry.getValue();
			if (calendarDto == null) {
				final Integer key = entry.getKey();
				final String dayOfWeekValue = DateTimeUtils.dayOfWeekMap.get(key);
				final Date date = DateTimeUtils.getDateByDateThAndWeek(key, week, year);
				calendarDto = new CalendarDto(dayOfWeekValue, date, new ArrayList<>(), new ArrayList<>());
			}
			this.objList.add(calendarDto);
		}
	}

	private void processCalendar(Map<Integer, CalendarDto> calendarByDayOfWeek, Calendar2 calendar2) {
		Date startTime = DateTimeUtils.compare2DateTime(calendar2.getStartTime(), frDate) > 0 ?
				calendar2.getStartTime() : frDate;
		Date endTime = DateTimeUtils.compare2DateTime(calendar2.getEndTime(), toDate) < 0 ?
				calendar2.getEndTime() : toDate;

		List<Date> timeSteps = createTimeSteps(startTime, endTime);
		for (Date step : timeSteps) {
			int dayOfWeek = DateTimeUtils.getDayOfWeek(step);
			calendarByDayOfWeek.compute(dayOfWeek, (k, v) -> {
				if (v == null) {
					v = new CalendarDto(DateTimeUtils.dayOfWeekMap.get(k), step, new ArrayList<>(), new ArrayList<>());
				}

				Calendar2DetailDto detailDto = new Calendar2DetailDto(calendar2, step);
				if (DateTimeUtils.isAmPM(step)) {
					v.getAmList().add(detailDto);
				} else {
					v.getPmList().add(detailDto);
				}

				return v;
			});
		}
	}

	private List<Date> createTimeSteps(Date startTime, Date endTime) {
		List<Date> timeSteps = new ArrayList<>();
		Date cursorTime = null;
		while (cursorTime == null ||
				(DateTimeUtils.compare2DateTime(startTime, cursorTime) <= 0 && DateTimeUtils.compare2DateTime(cursorTime, endTime) <= 0)) {
			if (cursorTime == null) {
				cursorTime = startTime;
			}
			timeSteps.add(cursorTime);

			// add 12 hours to cursor time
			Calendar c = Calendar.getInstance();
			c.setTime(cursorTime);
			c.add(Calendar.HOUR, 12);
			cursorTime = c.getTime();
		}

		// add end date
		final int lastIndex = timeSteps.size() - 1;
		final Date lastStep = timeSteps.get(lastIndex);
		if (DateTimeUtils.compare2DateTime(lastStep, endTime) < 0) { // do not add endTime yet
			if (DateUtils.isSameDay(lastStep, endTime)) {
				if (DateTimeUtils.isAmPM(lastStep) != DateTimeUtils.isAmPM(endTime)) {
					timeSteps.add(endTime);
				} else {
					if (lastIndex > 0) { // we only use endTime instead of last step if last step is not startTime
						timeSteps.set(lastIndex, endTime);
					}
				}
			} else {
				timeSteps.add(endTime);
			}
		}
		return timeSteps;
	}

	public Map<Integer, List<Calendar2>> setCalendarListByDate(List<Calendar2> all) {
		if(BussinessCommon.isEmptyList(all)) return calendarListByDate;
		for (Calendar2 i : all) {
			int key = DateTimeUtils.getDayOfWeek(i.getStartTime());
			if (!calendarListByDate.containsKey(key)) {
				calendarListByDate.put(key, new ArrayList<>());
			}
			calendarListByDate.get(key).add(i);
		}
		return calendarListByDate;
	}
}
