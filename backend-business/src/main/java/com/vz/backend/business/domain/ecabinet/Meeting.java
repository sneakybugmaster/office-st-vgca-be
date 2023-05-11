package com.vz.backend.business.domain.ecabinet;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.PrePersist;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.core.domain.Organization;
import org.apache.commons.lang.StringUtils;
import org.hibernate.annotations.Where;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.vz.backend.business.config.ecabinet.MeetingStatusEnum;
import com.vz.backend.business.domain.RoomMeeting;
import com.vz.backend.business.dto.MeetingCalendarDto;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Phiên họp
 *
 */
@Entity
@Table(name = "MEETING", schema = "vz")
@NoArgsConstructor
@AllArgsConstructor
@Data
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
public class Meeting extends BaseModel {

	/**
	 * Kỷ yếu
	 */
	@Column(name = "year_book_id")
	private Long yearBookId;
	@JsonIgnore
	@JoinColumn(name = "year_book_id", insertable = false, updatable = false)
	@ManyToOne(fetch = FetchType.LAZY)
	private YearBook yearBook;

	/**
	 * Phiên họp
	 */
	@Column(name = "subject")
	private String subject;

	/**
	 * Loại phiên họp
	 */
	@Column(name = "type_id")
	private Long typeId;
	@JsonIgnore
	@JoinColumn(name = "type_id", insertable = false, updatable = false)
	@ManyToOne(fetch = FetchType.LAZY)
	private Category type;

	/**
	 * Giấy mời họp
	 */
	@Column(name = "inv")
	private String inv;

	/**
	 * Thời gian họp từ
	 */
	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY_HH_MM_SS, timezone = "Asia/Ho_Chi_Minh")
	@Column(name = "start_date")
	private Date start;

	/**
	 * Thời gian họp đến
	 */
	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY_HH_MM_SS, timezone = "Asia/Ho_Chi_Minh")
	@Column(name = "end_date")
	private Date end;

	/**
	 * Địa điểm
	 */
	@Column(name = "place_id", nullable = true)
	private Long placeId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "place_id", insertable = false, updatable = false)
	private RoomMeeting roomMeeting;

	/**
	 * Trong trường hợp thêm mới
	 */
	@Transient
	private String place;

	/**
	 * Đề nghị
	 */
	@Column(name = "suggestion", columnDefinition = "TEXT")
	private String suggestion;

	/**
	 * Chương trình họp
	 */
	@Column(name = "schedule", columnDefinition = "TEXT")
	private String schedule;

	/**
	 * Chương trình họp Phân loại : Tệp tin/ text
	 */
	@Column(name = "scd_type")
	private String scdType;

	/**
	 * Thành phần tham dự
	 */
	@OneToMany(mappedBy = "meetingId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active='TRUE'")
	private List<Participant> participants;

	/**
	 * Nội dung họp
	 */
	@OneToMany(mappedBy = "meetingId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active='TRUE'")
	private List<Agenda> agendas;

	/**
	 * Trạng thái
	 */
	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private MeetingStatusEnum status;

	/**
	 * Diễn biến cuộc họp
	 */
	@Column(name = "progressContent", columnDefinition = "TEXT")
	private String progressContent;

	/**
	 * Kết luận cuộc họp
	 */
	@Column(name = "conclusion", columnDefinition = "TEXT")
	private String conclusion;

	/**
	 * Người tạo phiên họp
	 */
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "create_by", insertable = false, updatable = false)
	private User creator;

	@JsonProperty(value = "dInList")
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "meetingId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active = 'TRUE' AND type = 'VAN_BAN_DEN'")
	private List<DocumentMeeting> dInList = new ArrayList<>();
	
	@JsonProperty(value = "dOutList")
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "meetingId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active = 'TRUE' AND type = 'VAN_BAN_DI'")
	private List<DocumentMeeting> dOutList = new ArrayList<>();

	/**
	 * Cuộc họp từ Office
	 */
	@Column(name = "calendar_id", nullable = true)
	private Long calendarId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "calendar_id", insertable = false, updatable = false)
	private Calendar2 calendarModel;

	/**
	 * Cơ quan chủ trì
	 */
	@Column(name = "host_org_id", nullable = true)
	private Long hostOrgId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "host_org_id", insertable = false, updatable = false)
	private Organization hostOrg;

	/**
	 * Người chủ trì
	 */
	@Column(name = "host_user_id", nullable = true)
	private Long hostUserId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "host_user_id", insertable = false, updatable = false)
	private User hostUser;

	/**
	 * Ghi chú
	 */
	@Column(name = "note", nullable = true)
	private String note;

	@Override
	public void valids() {
		BussinessCommon.require("Phiên họp", this.subject);
		BussinessCommon.require("Loại phiên họp", this.typeId);
		BussinessCommon.require("Giấy mời họp", this.inv);
		BussinessCommon.require("Thời gian bắt đầu", this.start);
		BussinessCommon.require("Thời gian kết thúc", this.end);
		BussinessCommon.require("Chương trình họp", this.scdType);
		BussinessCommon.validLengthData(this.subject, "Phiên họp", 200);
		BussinessCommon.validLengthData(this.suggestion, "Đề nghị", 500);
		BussinessCommon.validLengthData(this.schedule, "Chương trình họp", 1000);

		if (StringUtils.isEmpty(this.place) && this.placeId == null) {
			throw new RestExceptionHandler("Địa điểm cuộc họp bắt buộc phải nhập");
		}

		if (!Constant.FILE.equals(this.scdType) && !Constant.TEXT.equals(this.scdType)) {
			throw new RestExceptionHandler("Thông tin chương trình họp không hợp lệ");
		}

		if (this.end.getTime() < this.start.getTime()) {
			throw new RestExceptionHandler("Thời gian kết thúc phải sau thời gian bắt đầu");
		}

		validAgendas();

		if (!BussinessCommon.isEmptyList(this.participants)) {
			this.participants.forEach(Participant::valids);
		}
	}

	public void validAgendas() {
		if (!BussinessCommon.isEmptyList(this.agendas)) {
			long minEndDate = 0l;
			for (Agenda i : this.agendas) {
				i.valids();
				long start = 0l;
				long end = 0l;

				if (i.getStart() != null) {
					start = i.getStart().getTime();
				}

				if (i.getEnd() != null) {
					end = i.getEnd().getTime();
				}

				if (start > 0 && start < this.start.getTime()) {
					throw new RestExceptionHandler(
							"Thời gian bắt đầu nội dung họp không nằm trong khoảng thời gian diễn ra phiên họp");
				}

				if (end > this.end.getTime()) {
					throw new RestExceptionHandler(
							"Thời gian kết thúc nội dung họp không nằm trong khoảng thời gian diễn ra phiên họp");
				}

				if ((end > 0 && minEndDate == 0l) || (end > 0 && end < minEndDate)) {
					minEndDate = end;
					continue;
				}

				if (start > 0 && start < minEndDate) {
					throw new RestExceptionHandler(
							"Khoảng thời gian họp đang bị lồng nhau trong quá trình diễn ra phiên họp");
				}
			}
		}
	}

	public void set(Meeting meeting) {
		this.yearBookId = meeting.getYearBookId();
		this.subject = meeting.getSubject();
		this.typeId = meeting.getTypeId();
		this.inv = meeting.getInv();
		this.start = meeting.getStart();
		this.end = meeting.getEnd();
		this.placeId = meeting.getPlaceId();
		this.suggestion = meeting.getSuggestion();
		this.schedule = meeting.getSchedule();
		this.scdType = meeting.getScdType();
	}

	@PrePersist
	public void prePersit() {
		if (this.status == null) {
			this.status = MeetingStatusEnum.MOI_TAO;
		}
	}
	
	public void setDInList(List<DocumentMeeting> docInList, Long meetingId) {
		if (!docInList.isEmpty()) {
			docInList.forEach(i -> {
				i.setMeetingId(meetingId);
				this.dInList.add(i);
			});
		}
	}

	public void setDOutList(List<DocumentMeeting> docOutList, Long meetingId) {
		if (!docOutList.isEmpty()) {
			docOutList.forEach(i -> {
				i.setMeetingId(meetingId);
				this.dOutList.add(i);
			});
		}
	}

	public void setCalendarToMeeting(MeetingCalendarDto c) {
		this.subject = c.getTitle();
		this.start = c.getStartTime();
		this.end = c.getEndTime();
		this.suggestion = c.getNote();
		if (c.getIsScheduleAFile() != null && Boolean.TRUE.equals(false)) {
			this.schedule = c.getDescription();
		}
	}
	
	

}
