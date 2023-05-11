package com.vz.backend.core.dto;

import java.util.Date;

import org.springframework.data.domain.Page;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class StraceDto {
	private long no;
	private String content;
	@JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "dd/MM/yyyy HH:mm:ss", timezone = DateTimeUtils.TIME_ZONE_ID)
	private Date createDate;
	private String userName;
	private String category;
	private String ipDevice;
	private String nameDevice;
	private String action;

	public StraceDto(String content, Date createDate, String userName, String category, String ipDevice,
			String nameDevice, String action) {
		this.content = content;
		this.createDate = createDate;
		this.userName = userName;
		this.category = category;
		this.ipDevice = ipDevice;
		this.nameDevice = nameDevice;
		this.action = action;
	}

	public static Page<StraceDto> convert(Page<StraceDto> pageRs) {
		if (!BussinessCommon.isEmptyPage(pageRs)) {
			long i = pageRs.getPageable().getOffset() + 1;
			for (StraceDto s : pageRs.getContent()) {
				s.setNo(i++);
				s.setAction(BussinessCommon.getAction(s.getAction()));
			}
		}
		return pageRs;
	}
}
