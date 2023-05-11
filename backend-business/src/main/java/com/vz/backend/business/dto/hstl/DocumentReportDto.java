package com.vz.backend.business.dto.hstl;

import java.util.Calendar;
import java.util.Date;

import com.vz.backend.business.dto.fullreport.ReportKey;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.ToString;

@Getter
@AllArgsConstructor
@ToString
public class DocumentReportDto {
	private Long id;
	private Date createDate;
	private DocumentTypeEnum docType;

	public ReportKey getKey(int groupMonth) {
		return DocumentReportDto.getKey(this.createDate, groupMonth);
	}

	public static ReportKey getKey(Date time, int groupMonth) {
		if (groupMonth <= 0) {
			throw new RestExceptionHandler("Group month must be >0");
		}
		Calendar cal = Calendar.getInstance(DateTimeUtils.timeZone());
		cal.setTime(time);

		int year = cal.get(Calendar.YEAR);
		int part = cal.get(Calendar.MONTH) / groupMonth + 1;
		String key = year + "-" + (part >= 10 ? "" : "0") + part;
		return new ReportKey(key, year, part);
	}
}
