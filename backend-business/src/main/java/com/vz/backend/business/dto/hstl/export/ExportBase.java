package com.vz.backend.business.dto.hstl.export;

import java.util.Calendar;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class ExportBase {
	private long total;
	private String date;
	private String month;
	private String year;
	private String orgNameUpper;
	private String orgNameLower;
	
	public ExportBase() {
		Organization org = BussinessCommon.getUser().getOrgModel();
		this.orgNameUpper = org.getName().toUpperCase();
		this.orgNameLower = org.getName();
		Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
		this.date = String.valueOf(c.get(Calendar.DATE));
		this.month = String.valueOf(c.get(Calendar.MONTH) + 1);
		this.year = String.valueOf(c.get(Calendar.YEAR));
	}
}
