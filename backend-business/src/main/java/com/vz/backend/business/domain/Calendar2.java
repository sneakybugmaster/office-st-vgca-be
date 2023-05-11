package com.vz.backend.business.domain;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.*;

import com.vz.backend.business.domain.ecabinet.AttachmentMeeting;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.OrgGroupDto;
import com.vz.backend.core.dto.UserBasicDto;
import lombok.*;
import org.hibernate.annotations.Where;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.vz.backend.business.domain.ecabinet.Meeting;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.CalendarStatusEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;

@Entity
@Table(name = "CALENDAR2", schema = "vz", indexes = {
		@Index(name = "idx_calendar2_id_org_id", columnList = "id, org_id, org_meeting, cat_type_id, chairman_id, room_id")
})
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class Calendar2 extends BaseModel {

	private static final long serialVersionUID = 1L;
//	@Column(name = "title")
	@Column(columnDefinition = "TEXT", name = "title")
	private String title;
	
	@Column(name = "address")
	private String address;
	
	@Column(name = "org_id")
	private Long orgId;
	@ManyToOne
	@JoinColumn(name = "org_id", insertable = false, updatable = false)
	private Organization orgModel;
	
	@Column(columnDefinition = "TEXT", name = "description")
	private String description;
	
	@Column(name = "start_time")
	@JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm", timezone = "Asia/Ho_Chi_Minh")
	private Date startTime;
	
	@Column(name = "end_time")
	@JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm", timezone = "Asia/Ho_Chi_Minh")
	private Date endTime;
	
	@Column(name = "publish_by")
	private Long publishBy;
	
	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private CalendarStatusEnum status;
	
	@Column(name = "register_ban")
	private Boolean registerBan;
	
	@Column(columnDefinition = "TEXT", name = "comment")
	private String comment;
	
//	@Column(name = "ingredient")
	@Column(columnDefinition = "TEXT", name = "ingredient")
	private String ingredient;
	
	@Column(name = "note")
	private String note;
	
	@Column(name = "is_unit_calendar")
	private boolean isUnitCalendar;
	
	@Column(name = "is_meeting_calendar", nullable = false)
	private boolean isMeetingCalendar = false;
	
	private String participants;

	@Column(name = "participants_guest")
	private String participantsGuest;

	@Column(name = "org_meeting")
	private Long orgMeeting;
	@ManyToOne
	@JoinColumn(name = "org_meeting", insertable = false, updatable = false)
	private Organization orgMeetingModel;

	@Column(name = "cat_type_id")
	private Long catTypeId;

	@Column(name = "chairman_id")
	private Long chairmanId;
	@ManyToOne
	@JoinColumn(name = "chairman_id", insertable = false, updatable = false)
	private User chairman;

	@Transient
	private List<UserBasicDto> participantsPersonal;
	@Transient
	private List<UserBasicDto> participantsGuests;
	@Transient
	private List<OrgGroupDto> participantsOrg;
	@Transient
	private List<OrgGroupDto> participantsGroup;

	@Column(name = "room_id")
	private Long roomId;
	@ManyToOne
	@JoinColumn(name = "room_id", insertable = false, updatable = false)
	private RoomMeeting roomMeeting;
	
	@Transient
	List<AttachmentCalendar> attachments;

	@Transient
	AttachmentMeeting contentFile;
	
	@Transient
	AttachmentCalendar attCalWeek;

	@Transient
	private Boolean isShowAttachments;

	@JsonProperty(value = "dInList")
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "cId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active = 'TRUE' AND type = 'VAN_BAN_DEN'")
	private List<DocumentCalendar> dInList = new ArrayList<>();
	
	@JsonProperty(value = "dOutList")
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "cId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active = 'TRUE' AND type = 'VAN_BAN_DI'")
	private List<DocumentCalendar> dOutList = new ArrayList<>();
	
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "cId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active = 'TRUE' AND type = 'GIAO_VIEC'")
	private List<DocumentCalendar> taskList = new ArrayList<>();

	@Column(name = "total_participants")
	private Integer totalParticipants; // Tổng số người tham gia

	/**
	 * Yêu cầu phòng họp
	 */
	@Column(name = "request_sound")
	private Boolean requestSound; // Hệ thống âm thanh
	@Column(name = "request_screen")
	private Boolean requestScreen; // Màn chiếu, máy chiếu
	@Column(name = "request_water")
	private Boolean requestWater; // Nước

	/**
	 * Họp ngoại viện
	 */
	@Transient
	private Boolean isOutside;
	
	public void validCalender() {
		// valid time
		if (this.getStartTime() == null || this.getEndTime() == null
				|| this.getEndTime().getTime() <= this.getStartTime().getTime()) {
			throw new RestExceptionHandler(Message.CALENDAR_INVALID_TIME);
		}
		// valid title
		if (StringUtils.isNullOrEmpty(this.getDescription())) {
			throw new RestExceptionHandler(Message.CALENDAR_INVALID_DESCRIPTION);
		}

		// valid length
		BussinessCommon.validLengthData(this.getDescription(), "Nội dung", 2000);
		BussinessCommon.validLengthData(this.getAddress(), "Địa điểm", 200);
		BussinessCommon.validLengthData(this.getNote(), "Ghi chú", 200);
	}
	
	public void validMeetingCalendar() {
		if(!this.isOutside && this.getRoomId() == null) {
			throw new RestExceptionHandler(Message.ROOM_INVALID);
		} else if(this.isOutside && this.getAddress() == null) {
			throw new RestExceptionHandler(Message.ADDRESS_OUTSIDE_INVALID);
		}
		
		BussinessCommon.validLengthData(this.getIngredient(), "Thành phần", 500);
	}

	/**
	 * @param news
	 * @param addCalendar : when user register calendar to Ban -> add calendar
	 */
	public void setCalendater(boolean isChangeStatus, Calendar2 news) {
		this.startTime = news.startTime;
		this.endTime = news.endTime;
		this.title = news.title;
		this.address = news.address;
		this.description = news.description;
		this.ingredient = news.ingredient;
		this.note = news.note;
		this.catTypeId = news.catTypeId;
		this.chairmanId = news.chairmanId;
		this.orgMeeting = news.orgMeeting;
		this.roomId = news.roomId;
		this.registerBan = news.registerBan;
		this.participants = news.participants;
		this.participantsPersonal = news.participantsPersonal;
		this.participantsOrg = news.participantsOrg;
		this.participantsGuest = news.participantsGuest;
		this.participantsGuests = news.participantsGuests;
		this.participantsGroup = news.participantsGroup;
		this.totalParticipants = news.totalParticipants;
		this.setStatus(!isChangeStatus ? this.status : CalendarStatusEnum.PRE_APPROVE);
	}
	
	public void setDInList(List<DocumentCalendar> docInList, Long cId) {
		if (!docInList.isEmpty()) {
			docInList.forEach(i -> {
				i.setCId(cId);
				this.dInList.add(i);
			});
		}
	}

	public void setDOutList(List<DocumentCalendar> docOutList, Long cId) {
		if (!docOutList.isEmpty()) {
			docOutList.forEach(i -> {
				i.setCId(cId);
				this.dOutList.add(i);
			});
		}
	}
	
	public void setTaskList(List<DocumentCalendar> taskList, Long cId) {
		if (!taskList.isEmpty()) {
			taskList.forEach(i -> {
				i.setCId(cId);
				this.taskList.add(i);
			});
		}
	}

	@PrePersist
	public void prePersit() {
		this.setActive(true);
		this.status = CalendarStatusEnum.PRE_APPROVE;
		if(this.registerBan == null) this.registerBan = false;
	}
	
	@PreUpdate
	public void preUpdate() {
		if(CalendarStatusEnum.APPROVE.equals(this.status)) {
			this.publishBy = BussinessCommon.getUserId();
		}
		if(this.registerBan == null) this.registerBan = false;
	}

	public String getStatusName() {
		return this.status != null ? status.getName() : "";
	}
	
	@Transient
	private boolean showApproveBt;
	
	@Transient
	private boolean showRejectBt;
	
	@Transient
	private boolean showEditBt;
	
	@Transient
	private boolean showCancelBt;
	
	@Transient
	private boolean showDelBt;
	
	@Transient
	private String parentOrgName;
	
	@Transient
	private String createUserName;
	
	@Transient
	private Boolean isCabinet;
	
	@Transient
	private Boolean isScheduleAFile;
	
	@Transient
	private String scheduleFileName;
	
	public String getStartTimeStr() {
		return DateTimeUtils.convertDateToStringPattern(this.startTime, DateTimeUtils.DD_MM_YYYY_HH_MM);
	}

	public String getEndTimeStr() {
		return DateTimeUtils.convertDateToStringPattern(this.endTime, DateTimeUtils.DD_MM_YYYY_HH_MM);
	}

	public Calendar2(Meeting m) {
		super();
		this.setId(m.getId());
		this.title = m.getSubject();
		this.startTime = m.getStart();
		this.endTime = m.getEnd();
		this.address = m.getRoomMeeting().getName();
		this.note = m.getSuggestion();
		if (BussinessCommon.convert(m.getScdType()).equals("file")) {
			this.description = StringUtils.isNullOrEmpty(m.getSchedule()) ? ""
					: FilesStorageService.origin(m.getSchedule());
			this.scheduleFileName = StringUtils.isNullOrEmpty(m.getSchedule()) ? "" : m.getSchedule();
			this.isScheduleAFile = true;
		} else {
			this.description = m.getSchedule();
			this.isScheduleAFile = false;
		}
		this.createUserName = m.getCreator().getFullName();
		this.isCabinet = true;
		this.status = CalendarStatusEnum.APPROVE;
	}
	
	public void clear() {
		this.taskList = new ArrayList<>();
		this.dInList = new ArrayList<>();
		this.dOutList = new ArrayList<>();
	}
}
