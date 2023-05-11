package com.vz.backend.business.dto;

import java.io.Serializable;
import java.math.BigInteger;
import java.util.Date;

import lombok.Getter;

@Getter
public class ResultQuickSearchDto implements Serializable {
	private static final long serialVersionUID = 1L;
	private Long id;
	private String code;
	private String name;
	private Date createDate;
	private String type;

	public ResultQuickSearchDto(Object id, String code, String name, Object createDate, String type) {
		super();
		this.id = ((BigInteger) id).longValue();
		this.code = convert(code);
		this.name = convert(name);
		this.type = type;
		if (createDate != null) {
			this.createDate = new java.util.Date(((java.sql.Date) createDate).getTime());
		}
	}

	private String convert(String s) {
		return s == null || s.isEmpty() || s.trim().length() == 0 ? "" : s;
	}
}
