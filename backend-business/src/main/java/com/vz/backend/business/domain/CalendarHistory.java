package com.vz.backend.business.domain;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.CalendarActionEnum;
import com.vz.backend.core.config.CalendarStatusEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.util.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "CALENDAR_HISTORY", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class CalendarHistory {
	
	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private Long id;
	
	@Column(name = "calendar_id", nullable = false)
	private Long calendarId;

	@Column(name = "user_id", nullable = false)
	private Long userId;
	@JsonIgnore
	@OneToOne
	@JoinColumn(name = "user_id", insertable = false, updatable = false)
	private User user;

	@Column(name = "date_create")
	private Date dateCreate;

	@Column(name = "action")
	@Enumerated(EnumType.STRING)
	private CalendarActionEnum action;

	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private CalendarStatusEnum status;

	@Column(name = "register_ban")
	private Boolean registerBan;

	@Column(columnDefinition = "TEXT", name = "comment")
	private String comment;

//	@Column(name = "ingredient")
	@Column(columnDefinition = "TEXT", name = "participants")
	private String participants;

	@Column(columnDefinition = "TEXT", name = "participants_group")
	private String participantsGroup;

	@Column(columnDefinition = "TEXT", name = "participants_guest")
	private String participantsGuest;

	@Column(columnDefinition = "TEXT", name = "participants_org")
	private String participantsOrg;

//	@Column(name = "note")
	@Column(columnDefinition = "TEXT", name = "note")
	private String note;

	@Column(columnDefinition = "TEXT", name = "description")
	private String description;

	@Column(name = "start_time")
	@JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm", timezone = "Asia/Ho_Chi_Minh")
	private Date startTime;

	@Column(name = "end_time")
	@JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm", timezone = "Asia/Ho_Chi_Minh")
	private Date endTime;

//	@Column(name = "title")
	@Column(columnDefinition = "TEXT", name = "title")
	private String title;

//	@Column(name = "address")
	@Column(columnDefinition = "TEXT", name = "address")
	private String address;
	
	@PrePersist
	public void prePersit() {
		this.userId = BussinessCommon.getUserId();
		this.dateCreate = new Date();
	}

	public static CalendarHistory set(Calendar2 c, CalendarActionEnum action) {
		return new CalendarHistory(c.getId(), action, c.getStatus(), c.getRegisterBan(), c.getComment(),
				c.getParticipants(), c.getNote(), c.getDescription(), c.getStartTime(), c.getEndTime(), c.getTitle(),
				c.getAddress());
	}
	
	public CalendarHistory(Long calendarId, CalendarActionEnum action, CalendarStatusEnum status, Boolean registerBan,
			String comment, String participants, String note, String description, Date startTime, Date endTime,
			String title, String address) {
		this.calendarId = calendarId;
		this.action = action;
		this.status = status;
		this.registerBan = registerBan;
		this.comment = comment;
		this.participants = participants;
		this.note = note;
		this.description = description;
		this.startTime = startTime;
		this.endTime = endTime;
		this.title = title;
		this.address = address;
	}
	
	public String getUserName() {
		return this.user != null ? user.getFullName() : "";
	}
	
	public String getActionStr() {
		return this.action != null ? action.getName() : "";
	}
	
	public String getStatusStr() {
		return this.status != null ? status.getName() : "";
	}
	
	public String getStartTimeStr() {
		return DateTimeUtils.convertDateToStringPattern(this.startTime, DateTimeUtils.DD_MM_YYYY_HH_MM);
	}

	public String getEndTimeStr() {
		return DateTimeUtils.convertDateToStringPattern(this.endTime, DateTimeUtils.DD_MM_YYYY_HH_MM);
	}
}
