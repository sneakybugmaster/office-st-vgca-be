package com.vz.backend.business.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.ToString;

@AllArgsConstructor
@Getter
@ToString
public class MatchScheduleDto {
	private Long clientId;
	private Long userId;
	private Long scheduleId;
	private String preview;
}
