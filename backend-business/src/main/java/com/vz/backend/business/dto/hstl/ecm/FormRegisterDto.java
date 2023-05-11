package com.vz.backend.business.dto.hstl.ecm;

import java.util.Date;

import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;

@Getter
public class FormRegisterDto {
	private Long id;
	private String name;
	
	public FormRegisterDto(Long id, String name, Date dateResponse) {
		this.id = id;
		this.name = dateResponse == null ? name
				: name + "-" + DateTimeUtils.convertDateToStringPattern(dateResponse, DateTimeUtils.YYYY_MM_DD);
	}
}
