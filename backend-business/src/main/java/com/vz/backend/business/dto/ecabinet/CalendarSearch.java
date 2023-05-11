package com.vz.backend.business.dto.ecabinet;

import java.util.Calendar;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
public class CalendarSearch {

	enum Type {
		MONTH, WEEK, DATE
	}

	private String text;

	/**
	 * Tháng /Tuần/ Ngày
	 */
	private Type type;

	/**
	 * Tháng /Tuần/ Ngày
	 */
	private int value;

	/**
	 * Năm
	 */
	private Integer year;

	/**
	 * Ngày
	 */
	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY)
	private Date date;

	// convert to
	@JsonIgnore
	private Date start;
	
	@JsonIgnore
	private Date end;

	@JsonIgnore
	private Long userId;

	public void convert() {
		this.handleTime();
		this.text = BussinessCommon.convert(text);
		this.userId = BussinessCommon.getUserId();
	}

	private void handleTime() {
		BussinessCommon.require("Loại thời gian", this.type);
		if(year == null) {
			Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
			year = c.get(Calendar.YEAR);
		}
		
		Date from = null;
		Date to = null;
		switch (this.type) {
		case MONTH:
			BussinessCommon.require("Tháng", this.value);
			from = DateTimeUtils.firstDateOfMonth(this.value, year);
			to = DateTimeUtils.lastDateOfMonth(this.value, year);
			break;
		case DATE:
			BussinessCommon.require("Ngày", this.date);
			from = this.date;
			to = this.date;
			break;
		case WEEK:
			BussinessCommon.require("Tuần", this.value);
			from = DateTimeUtils.getDateByWeek(this.value, year, 1);
			to = DateTimeUtils.getDateByWeek(this.value, year, 2);
			break;
		default:
			break;
		}

		this.start = DateTimeUtils.handleSubmit(from, Calendar.MILLISECOND, -1);
		this.end = DateTimeUtils.handleSubmit(to, Calendar.DAY_OF_MONTH, 1);
	}
}
