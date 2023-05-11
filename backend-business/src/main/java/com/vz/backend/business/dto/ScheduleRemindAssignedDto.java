package com.vz.backend.business.dto;

import com.vz.backend.core.config.ObjectRemindedTypeEnum;

import lombok.Getter;

@Getter
public class ScheduleRemindAssignedDto extends ScheduleRemindDto {

	private boolean active;

	public ScheduleRemindAssignedDto(Long id, String description, String hours, String dayOfWeek, String date,
			String month, String position, String userId, Boolean active, boolean df, ObjectRemindedTypeEnum objType, String objId) {
		super(id, description, hours, dayOfWeek, date, month, position, userId, objType, objId);
		this.active = !Boolean.TRUE.equals(active);
	}
	
	public ScheduleRemindAssignedDto(Long id, String description, String hours, String dayOfWeek, String date,
			String month, String position, String userId, Boolean active, ObjectRemindedTypeEnum objType, String objId) {
		super(id, description, hours, dayOfWeek, date, month, position, userId, objType, objId);
		this.active = !Boolean.FALSE.equals(active);
	}
}
