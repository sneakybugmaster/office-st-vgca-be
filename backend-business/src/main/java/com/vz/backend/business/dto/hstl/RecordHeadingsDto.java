package com.vz.backend.business.dto.hstl;

import java.util.Calendar;
import java.util.List;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Getter
@Setter
public class RecordHeadingsDto {
	private String date;
	private String month;
	private String year;
	private String orgNameUpper;
	private String orgNameLower;
	private long total;
	private long totalUnlimit;
	private long totalLimit;
	private List<RecordHeadings> contents;
	
	public RecordHeadingsDto(List<RecordHeadings> content) {
		this.contents = content;
		this.total = content.stream().filter(i -> !("").equals(i.getMaintenance())).count();
		this.totalLimit = content.stream().filter(i -> i.getLimitation() != null && i.getLimitation()).count();
		this.totalUnlimit = Math.abs(this.total - this.totalLimit);
		this.orgNameUpper = BussinessCommon.getUser().getOrgModel().getName().toUpperCase();
		this.orgNameLower = BussinessCommon.getUser().getOrgModel().getName();
		Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
		this.date = String.valueOf(c.get(Calendar.DATE));
		this.month = String.valueOf(c.get(Calendar.MONTH) + 1);
		this.year = String.valueOf(c.get(Calendar.YEAR));
	}
}