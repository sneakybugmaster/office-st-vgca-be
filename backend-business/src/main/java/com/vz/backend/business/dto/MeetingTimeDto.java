package com.vz.backend.business.dto;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Data;

@AllArgsConstructor
@Data
public class MeetingTimeDto {
	private Long roomId;
	private Date startDate;
	private Date endDate;
}
