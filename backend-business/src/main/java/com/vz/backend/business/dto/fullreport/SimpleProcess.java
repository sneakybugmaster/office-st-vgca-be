package com.vz.backend.business.dto.fullreport;

import java.util.Calendar;
import java.util.Date;

import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@AllArgsConstructor
public class SimpleProcess {
	private Long id;
	private Long docId;
	@Setter
	private Date updateDate;

	public ReportKey getKey(int groupMonth) {
		if (groupMonth <= 0) {
			throw new RestExceptionHandler("Group month must be >0");
		}
		Calendar cal = Calendar.getInstance(DateTimeUtils.timeZone());
		cal.setTime(this.updateDate);

		int year = cal.get(Calendar.YEAR);
		int part = cal.get(Calendar.MONTH) / groupMonth + 1;
		String key = year + "-" + (part >= 10 ? "" : "0") + part;
		return new ReportKey(key, year, part);
	}
}
