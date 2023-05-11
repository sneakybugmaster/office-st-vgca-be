package com.vz.backend.core.config;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Value;

import com.vz.backend.core.dto.LabelValueDto;

public enum ObjectRemindedTypeEnum {
	
	DOCUMENT_IN("Văn bản đến"), DOCUMENT_OUT("Văn bản đi"), CALENDAR_MEETING("Lịch họp"), TASK("Công việc");
	
	public final String name;
	
	ObjectRemindedTypeEnum(String name) {
		this.name = name;
	}
	
	@Value("${configs.remind.remind_for_obj: false}")
	private boolean remindObj;
	
	public List<LabelValueDto<String>> get() {
		List<LabelValueDto<String>> data = new ArrayList<>();
		if (!remindObj) {
			data.add(new LabelValueDto<>(ObjectRemindedTypeEnum.CALENDAR_MEETING.toString(),
					ObjectRemindedTypeEnum.CALENDAR_MEETING.name));
			return data;
		}

		for (ObjectRemindedTypeEnum v : values()) {
			data.add(new LabelValueDto<>(v.toString(), v.name));
		}
		return data;
	}
}
