package com.vz.backend.business.domain.ecabinet;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.util.StringUtils;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.config.ecabinet.AttendanceTypeEnum;
import com.vz.backend.business.dto.ecabinet.AttendanceDto;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Điểm danh
 *
 */
@Entity
@Table(name = "ATTENDANCE", schema = "vz")
@Data
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
@AllArgsConstructor
@NoArgsConstructor
public class Attendance extends BaseModel {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "meeting_id")
	private Long meetingId;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "meeting_id", updatable = false, insertable = false)
	@JsonIgnore
	private Meeting meeting;

	@Column(name = "user_id")
	private Long userId;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", updatable = false, insertable = false)
	@JsonIgnore
	private User user;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private AttendanceTypeEnum type;

	@Column(name = "replace_user_id", nullable = true)
	private Long replaceUserId;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "replace_user_id", updatable = false, insertable = false)
	@JsonIgnore
	private User replaceUser;

	private String reason;

	@Column(nullable = false)
	@Enumerated(EnumType.STRING)
	private AttendanceTypeEnum invitation;

	private Boolean isAssigned;

	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY_HH_MM_SS, timezone = "Asia/Ho_Chi_Minh")
	private Date absentStart;

	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY_HH_MM_SS, timezone = "Asia/Ho_Chi_Minh")
	private Date absentEnd;

	@Override
	public void valids() {
		if (AttendanceTypeEnum.ABSENT.equals(this.type)) {
			BussinessCommon.require("Lý do", this.reason);
			BussinessCommon.require("Bắt đầu vắng", this.absentStart);
			BussinessCommon.require("Kết thúc vắng", this.absentEnd);
			BussinessCommon.validLengthData(this.reason, "Lý do", 500);
		} else if (!AttendanceTypeEnum.ABSENT.equals(this.type) && !StringUtils.isEmpty(this.reason)) {
			throw new RestExceptionHandler("Nếu không vắng mặt thì không nhập lý do");
		} else if (!AttendanceTypeEnum.ABSENT.equals(this.type) && (absentStart != null || absentEnd != null)) {
			throw new RestExceptionHandler("Nếu không vắng mặt thì không nhập thời gian vắng mặt ");
		}
	}

	public void set(Attendance attendance) {
		this.type = attendance.getType();
		this.replaceUserId = attendance.getReplaceUserId();
		this.reason = attendance.getReason();
		this.absentStart = attendance.getAbsentStart();
		this.absentEnd = attendance.getAbsentEnd();
	}
	
	public void setAbsent(AttendanceDto dto) {
		this.type = AttendanceTypeEnum.ABSENT;
		this.absentStart = dto.getAbsentStart();
		this.absentEnd = dto.getAbsentEnd();
		this.reason = dto.getReason();
		this.replaceUserId = dto.getReplaceUserId();
	}

	public Attendance(Long meetingId, Long userId, AttendanceTypeEnum type, AttendanceTypeEnum invitation,
			Boolean isAssigned) {
		super();
		this.meetingId = meetingId;
		this.userId = userId;
		this.type = type;
		this.invitation = invitation;
		this.isAssigned = isAssigned;
	}

}
