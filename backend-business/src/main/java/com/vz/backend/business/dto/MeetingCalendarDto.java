package com.vz.backend.business.dto;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.vz.backend.business.domain.ecabinet.AttachmentMeeting;
import com.vz.backend.business.domain.ecabinet.DocumentMeeting;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.dto.OrgGroupDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.DateTimeUtils;

import com.vz.backend.util.StringUtils;
import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class MeetingCalendarDto {
	private Long id;
	
	private String title;
	
	private String description;
	
	private Boolean isScheduleAFile;
	
	private String note;
	
	@JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm", timezone = "Asia/Ho_Chi_Minh")
	private Date startTime;
	
	@JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm", timezone = "Asia/Ho_Chi_Minh")
	private Date endTime;
	
	@JsonProperty("dInList")
	private List<DocumentMeeting> dInList;
	
	@JsonProperty("dOutList")
	private List<DocumentMeeting> dOutList;
	
	private Boolean isCabinet;
	
	private String participants;
	
	private List<OrgGroupDto> participantsOrg;
	
	private List<OrgGroupDto> participantsGroup;
	
	private List<AttachmentMeeting> attachments;
	
	public void validCalender() {
		// valid time
		if (this.getStartTime() == null || this.getEndTime() == null
				|| this.getEndTime().getTime() <= this.getStartTime().getTime()) {
			throw new RestExceptionHandler("Thời gian kết thúc phải sau thời gian bắt đầu");
		}

//		if (StringUtils.isNullOrEmpty(this.getParticipants())
//				&& BussinessCommon.isEmptyList(this.getParticipantsOrg())
//				&& BussinessCommon.isEmptyList(this.getParticipantsGroup())) {
//			throw new RestExceptionHandler("Lỗi chưa nhập thành phần tham gia");
//		}

		// valid length
		BussinessCommon.require("Tiêu đề", this.getTitle());
		BussinessCommon.validLengthData(this.getTitle(), "Tiêu đề", 500);
		BussinessCommon.validLengthData(this.getDescription(), "Nội dung", 1000);
		BussinessCommon.validLengthData(this.getNote(), "Ghi chú", 200);
	}
}
