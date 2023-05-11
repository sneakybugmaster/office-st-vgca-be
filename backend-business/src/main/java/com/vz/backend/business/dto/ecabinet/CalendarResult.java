package com.vz.backend.business.dto.ecabinet;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CalendarResult {

	private Long meetingId;
	
	/**
	 * Phiên họp
	 */
	private String subject;
	
	/**
	 * Địa điểm
	 */
	private String place;
	
	/**
	 * Thời gian họp từ
	 */
	private Date start;

	/**
	 * Thời gian họp đến
	 */
	private Date end;
}
