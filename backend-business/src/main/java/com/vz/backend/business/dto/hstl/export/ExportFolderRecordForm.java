package com.vz.backend.business.dto.hstl.export;

import java.util.Calendar;

import com.vz.backend.business.domain.hstl.HsFolderRecordForm;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordFormDto;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class ExportFolderRecordForm extends HsFolderRecordFormDto {
	private String date;
	private String month;
	private String year;
	private String orgNameUpper;
	private String orgNameLower;
	private String cachThucNopLuuName;
	private String nguonNopLuuName;
	
	public void convert(HsFolderRecordForm form, boolean changeFormat) {
		super.convert(form, changeFormat);
		Organization org = form.getOrg() == null ? BussinessCommon.getUser().getOrgModel() : form.getOrg();
		this.orgNameUpper = org.getName().toUpperCase();
		this.orgNameLower = org.getName();
		Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
		this.date = String.valueOf(c.get(Calendar.DATE));
		this.month = String.valueOf(c.get(Calendar.MONTH) + 1);
		this.year = String.valueOf(c.get(Calendar.YEAR));
		this.cachThucNopLuuName = form.getWay() == null ? "" : form.getWay().name;
		this.nguonNopLuuName = form.getSrc() == null ? "" : form.getSrc().name;
	}
}
