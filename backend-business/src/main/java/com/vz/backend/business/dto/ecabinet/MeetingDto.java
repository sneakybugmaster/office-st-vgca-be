package com.vz.backend.business.dto.ecabinet;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.config.ecabinet.MeetingStatusEnum;
import com.vz.backend.business.domain.ecabinet.Attendance;
import com.vz.backend.business.domain.ecabinet.Meeting;
import com.vz.backend.core.common.BussinessCommon;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Getter
public class MeetingDto {

	/**
	 * STT
	 */
	@Setter
	private int no;

	/**
	 * Chỉ mục phiên họp
	 */
	private Long meetingId;

	/**
	 * Thời gian bắt đầu
	 */
	private Date start;

	/**
	 * Thời gian kết thúc
	 */
	private Date end;

	/**
	 * Nội dung
	 */
	private String subject;

	/**
	 * Địa điểm
	 */
	private String place;

	/**
	 * Chủ trì cuộc họp
	 */
	@Setter
	private String host;

	/**
	 * Thành viên xác nhận
	 */
	@Setter
	private String numberConfirmedMember;

	/**
	 * Trạng thái phiên họp
	 */
	private String meetingStatus;

	/**
	 * Trạng thái người dùng đối với phiên họp
	 */
	@Setter
	private String userStatus;

	/**
	 * Loại tham dự : Mời cá nhân/ Mời tổ chức
	 */
	@Setter
	private String type;

	/**
	 * Trạng thái phiên họp
	 */
	@JsonIgnore
	private MeetingStatusEnum status;

	@JsonIgnore
	private Long createBy;

	/**
	 * Danh sách button
	 */
	@Setter
	private MeetingActionDto action = new MeetingActionDto();
	
	/**
	 * Thư kí 
	 */
	@Setter
	private Boolean main = false;
	
	/**
	 * Người được mời chính
	 */
	@JsonIgnore
	private Long mainId;

	/**
	 * Người được mời thay thế
	 */
	@JsonIgnore
	private Long replaceId;
	
	@JsonIgnore
	private Attendance at;
	
	public MeetingDto(Long meetingId, Date start, Date end, String subject, String place, MeetingStatusEnum status,
			Long createBy) {
		super();
		this.meetingId = meetingId;
		this.start = start;
		this.end = end;
		this.subject = subject;
		this.place = place;
		this.meetingStatus = status == null ? "" : status.name;
		this.status = status;
		this.createBy = createBy;
	}
	
	public MeetingDto(Meeting meeting, Attendance attendance) {
		super();
		this.meetingId = meeting.getId();
		this.start = meeting.getStart();
		this.end = meeting.getEnd();
		this.subject = meeting.getSubject();
		this.place = meeting.getRoomMeeting() == null ? "" : meeting.getRoomMeeting().getName();
		this.meetingStatus = meeting.getStatus() == null ? "" : meeting.getStatus().name;
		this.status = meeting.getStatus();
		this.createBy = meeting.getCreateBy();
		this.mainId = attendance == null ? null : attendance.getUserId();
		this.replaceId = attendance == null ? null : attendance.getReplaceUserId();
		this.at = attendance;
		this.main = attendance != null || BussinessCommon.getUserId().equals(meeting.getCreateBy());
	}
}