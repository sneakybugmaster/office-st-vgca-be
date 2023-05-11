package com.vz.backend.business.dto.ecabinet;

import java.util.Calendar;
import java.util.Date;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
public class MeetingSearchDto {
	/**
	 * Text
	 */
	private String text;

	/**
	 * Thời gian bắt đầu
	 */
	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY)
	private Date start;

	/**
	 * Thời gian kết thúc
	 */
	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY)
	private Date end;

	/**
	 * Nội dung họp
	 */
	private String content;

	/**
	 * Đơn vị chủ trì
	 */
	private Long hostOrgId;

	/**
	 * Người Chủ trì
	 */
	private Long hostUserId;

	/**
	 * Thành phần tham gia
	 */
	private Long memberId;
	
	/**
	 * Người hiện tại
	 */
	private Long userId;

	/**
	 * File tạm
	 */
	private String fileNameTmp;
	
	/**
	 * Tìm kiếm phiên họp từ màn hình dự thảo
	 */
	private Boolean draftScreen;
	
	/**
	 * Notification
	 */
	private Long meetingId;
	
	/**
	 * Boss
	 */
	private Long bossId;
	
	public void convert() {
		this.text = BussinessCommon.convert(this.text);
		this.start = DateTimeUtils.handleSubmit(this.start, Calendar.MILLISECOND, -1);
		this.end = DateTimeUtils.handleSubmit(this.end, Calendar.DAY_OF_MONTH, 1);
		this.content = BussinessCommon.convert(this.content);
		this.hostOrgId = BussinessCommon.convert(this.hostOrgId);
		this.hostUserId = BussinessCommon.convert(this.hostUserId);
		this.memberId = BussinessCommon.convert(this.memberId);
		this.userId = BussinessCommon.getUserId();
		this.meetingId = BussinessCommon.convert(meetingId);
	}
}
