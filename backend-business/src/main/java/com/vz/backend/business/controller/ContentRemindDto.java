package com.vz.backend.business.controller;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.core.common.BussinessCommon;

import lombok.Data;

@Data
public class ContentRemindDto {
	private Long objId;
	private String title;
	private String preview;
	private Date startDate;
	private Date endDate;
	
	public ContentRemindDto(Long objId, String title, String preview) {
		this.objId = objId;
		this.title = title;
		this.preview = preview;
	}
	
	public ContentRemindDto(Calendar2 c) {
		this.objId = c.getId();
		this.title = c.getTitle();
		this.preview = c.getAddress();
		this.startDate = c.getStartTime();
		this.endDate = c.getEndTime();
	}
	
	public static List<ContentRemindDto> getByCalendar(List<Calendar2> cList) {
		List<ContentRemindDto> rsList = new ArrayList<>();
		if(BussinessCommon.isEmptyList(cList)) return rsList;
		cList.forEach(i -> rsList.add(new ContentRemindDto(i)));
		return rsList;
	}
	
	public static List<ContentRemindDto> getDocumentIn(List<Documents> cList) {
		List<ContentRemindDto> rsList = new ArrayList<>();
		if(BussinessCommon.isEmptyList(cList)) return rsList;
		cList.forEach(i -> rsList.add(new ContentRemindDto(i.getId(), i.getPreview(), i.getNumberOrSign())));
		return rsList;
	}
	
	public static List<ContentRemindDto> getDocumentOut(List<DocumentOut> cList) {
		List<ContentRemindDto> rsList = new ArrayList<>();
		if(BussinessCommon.isEmptyList(cList)) return rsList;
		cList.forEach(i -> rsList.add(new ContentRemindDto(i.getId(), i.getPreview(), i.getNumberOrSign())));
		return rsList;
	}
	
}
