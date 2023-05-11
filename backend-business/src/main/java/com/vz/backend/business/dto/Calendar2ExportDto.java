package com.vz.backend.business.dto;

import java.util.Calendar;
import java.util.Date;

import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@NoArgsConstructor
public class Calendar2ExportDto {
	private Long orgType;
	private Date date;
	private Date startDate;
	private Date endDate;
	@Setter
	private Long orgId;
	private String something;

	public void setDate(Date date) {
		if (date == null) {
			date = new Date();
		}
		date = DateTimeUtils.handleSubmit(date);
		Calendar ca = Calendar.getInstance();
		ca.setTime(date);
		this.date = date;
		int day = ca.get(Calendar.DAY_OF_WEEK);
		ca.add(Calendar.DATE, 2 - day); // Monday
		startDate = ca.getTime();
		ca.add(Calendar.DATE, 7); // to the next Monday
		ca.add(Calendar.MILLISECOND, -1); // Sunday
		endDate = ca.getTime();
	}
}
