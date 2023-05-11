package com.vz.backend.business.domain;

import java.util.List;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.controller.ContentRemindDto;
import com.vz.backend.business.dto.ScheduleRemindDto;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.ObjectRemindedTypeEnum;
import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

// TODO: use cron parser

@Entity
@Table(name = "ScheduleRemind", schema = "vz")
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties({"active", "createDate", "updateDate", "createBy", "updateBy"})
public class ScheduleRemind extends BaseModel {
	private static final long serialVersionUID = 1L;
	private String description;
	private String hours;
	private String dayOfWeek;
	private String date;
	private String month;
	private String position;
	private String userId;
	
	@Enumerated(EnumType.STRING)
	private ObjectRemindedTypeEnum objType;
	
	private String objId;
	
	@Transient
	private List<ContentRemindDto> content;
	
	public static final String SEPARATOR = ",";
	
	public static final String ALL = "*";

	public static <T> String wrap(T id) {
		return SEPARATOR + id + SEPARATOR;
	}

	public ScheduleRemind(ScheduleRemindDto dto) {
		this.put(dto);
	}

	public void put(ScheduleRemindDto dto) {
		this.description = dto.getDescription();
		this.hours = BussinessCommon.joinOrStar(dto.getHours(), ALL);
		this.dayOfWeek = BussinessCommon.joinOrStar(dto.getDayOfWeek(), ALL);
		this.date = BussinessCommon.joinOrStar(dto.getDate(), ALL);
		this.month = BussinessCommon.joinOrStar(dto.getMonth(), ALL);
		this.position = BussinessCommon.joinOrStar(dto.getPosition(), ALL);
		this.userId = BussinessCommon.joinOrStar(dto.getUserId(), ALL);
		this.objType = dto.getObjType();
		this.objId = BussinessCommon.joinOrStar(dto.getObjId(), ALL);
//		this.relevantObjId = joinOrStar(dto.getRelevantObjId());
	}
}
