package com.vz.backend.business.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
public class CalendarDto {
	private String dateStr;
	private Date date;
	private List<Calendar2DetailDto> amList;
	private List<Calendar2DetailDto> pmList;
	private List<Calendar2DetailDto> listCalendar;
	private Boolean isToday;

	public CalendarDto(String dateStr, Date date, List<Calendar2DetailDto> amList, List<Calendar2DetailDto> pmList) {
		this.dateStr = dateStr;
		this.date = date;
		this.amList = amList;
		this.pmList = pmList;
	}

	public CalendarDto(String dateStr, Date date, List<Calendar2DetailDto> listCalendar, Boolean isToday) {
		this.dateStr = dateStr;
		this.date = date;
		this.listCalendar = listCalendar;
		this.isToday = isToday;
	}
}