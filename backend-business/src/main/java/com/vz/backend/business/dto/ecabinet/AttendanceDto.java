package com.vz.backend.business.dto.ecabinet;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.vz.backend.business.domain.ecabinet.Attendance;
import com.vz.backend.util.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class AttendanceDto {
	
	private Long userId;
	private Long meetingId;
	private Long replaceUserId;
	private String reason;
	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY_HH_MM_SS, timezone = "Asia/Ho_Chi_Minh")
	private Date absentStart;
	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY_HH_MM_SS, timezone = "Asia/Ho_Chi_Minh")
	private Date absentEnd;
	private List<Attendance> list;
}
