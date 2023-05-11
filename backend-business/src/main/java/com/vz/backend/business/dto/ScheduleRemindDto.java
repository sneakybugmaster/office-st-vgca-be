package com.vz.backend.business.dto;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Map;

import javax.persistence.Transient;

import com.google.common.collect.Iterables;
import com.vz.backend.business.controller.ContentRemindDto;
import com.vz.backend.business.domain.ScheduleRemind;
import com.vz.backend.core.config.ObjectRemindedTypeEnum;
import com.vz.backend.util.StringUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Getter
public class ScheduleRemindDto {
	private Long id;
	private String description;
	private List<Integer> hours;
	private List<Integer> dayOfWeek;
	private List<Integer> date;
	private List<Integer> month;
	private List<Long> position;
	private List<Long> userId;
	private ObjectRemindedTypeEnum objType;
	private List<Long> objId;
	@Setter
	private Map<Long, String> allUser;
	@Setter
	private Map<Long, String> allPosition;
	private List<ContentRemindDto> content;
	
	@Transient
	public String getObjTypeName() {
		return objType != null ? objType.name : "";
	}

	public static boolean validHour(Integer hour) {
		return hour != null && hour >= 6 && hour <= 18;
//		return hour != null && hour >= 0 && hour <= 24;
	}

	public static boolean validDayOfWeek(Integer dayOfWeek) {
		return dayOfWeek != null && dayOfWeek >= Calendar.SUNDAY && dayOfWeek <= Calendar.SATURDAY;
//		return dayOfWeek != null && dayOfWeek >= 1 && dayOfWeek <= 7;
	}

	public static boolean validDate(Integer date) {
		return date != null && date >= 1 && date <= 31;
	}

	public static boolean validMonth(Integer month) {
		return month != null && month >= 0 && month <= 11;
	}

	public static boolean validPositive(Long position) {
		return position != null && position > 0;
	}

	public boolean valid() {
		if (hours == null || dayOfWeek == null || date == null || month == null || position == null || userId == null) {
			return false;
		}
		return Iterables.all(this.hours, ScheduleRemindDto::validHour)
				&& Iterables.all(this.dayOfWeek, ScheduleRemindDto::validDayOfWeek) && Iterables.all(this.date,
						ScheduleRemindDto::validDate)
				&& Iterables.all(this.month, ScheduleRemindDto::validMonth)
				&& Iterables.all(this.userId, ScheduleRemindDto::validPositive)
				&& Iterables.all(this.position, ScheduleRemindDto::validPositive);
	}

	public ScheduleRemindDto(ScheduleRemind dto) {
		this.id = dto.getId();
		this.description = dto.getDescription();
		this.hours = toList(dto.getHours());
		this.dayOfWeek = toList(dto.getDayOfWeek());
		this.date = toList(dto.getDate());
		this.month = toList(dto.getMonth());
		this.position = toListLong(dto.getPosition());
		this.userId = toListLong(dto.getUserId());
		this.objId = toListLong(dto.getObjId());
		this.objType = dto.getObjType();
		this.content = dto.getContent();
	}

	public ScheduleRemindDto(Long id, String description, String hours, String dayOfWeek, String date, String month, String position, String userId, ObjectRemindedTypeEnum objType, String objId) {
		super();
		this.id = id;
		this.description = description;
		this.hours = toList(hours);
		this.dayOfWeek = toList(dayOfWeek);
		this.date = toList(date);
		this.month = toList(month);
		this.position = toListLong(position);
		this.userId = toListLong(userId);
		this.objId = toListLong(objId);
		this.objType = objType;
	}
	
	public ScheduleRemindDto(Long id, String description, String hours, String dayOfWeek, String date, String month, String position, String userId) {
		super();
		this.id = id;
		this.description = description;
		this.hours = toList(hours);
		this.dayOfWeek = toList(dayOfWeek);
		this.date = toList(date);
		this.month = toList(month);
		this.position = toListLong(position);
		this.userId = toListLong(userId);
	}

	private List<Integer> toList(String str) {
		if (str == null || "*".equals(str)) {
			return new ArrayList<>();
		}
		return StringUtils.parse(str, Integer::parseInt);
	}

	public static List<Long> toListLong(String str) {
		if (str == null || "*".equals(str)) {
			return new ArrayList<>();
		}
		return StringUtils.parse(str, Long::parseLong);
	}
}
