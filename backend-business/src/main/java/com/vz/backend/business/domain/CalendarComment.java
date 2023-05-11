package com.vz.backend.business.domain;

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "CALENDAR_COMMENT", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class CalendarComment extends BaseModel {

	private static final long serialVersionUID = 1L;

	@Column(name = "calendar_id")
	private Long calendarId;

	@Column(columnDefinition = "TEXT", name = "comment")
	private String comment;

	@Column(name = "user_id")
	private Long userId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "user_id", updatable = false, insertable = false)
	private User user;

//	@OneToMany(fetch = FetchType.LAZY, mappedBy = "objId")
//	@Where(clause = "objType = 'CALENDAR_CMT'")
	@Transient
	private List<AttachmentCalendar> attachments;
	
	public String getFullName() {
		return this.user != null ? user.getFullName() : "";
	}
	
	public String getPositionName() {
		return this.user != null ? user.getPositionModel().getName() : "";
	}
	
	@PrePersist
	public void prePersit() {
		this.userId = BussinessCommon.getUserId();
	}

	public CalendarComment(Long calendarId, String comment, Long userId) {
		this.calendarId = calendarId;
		this.comment = comment;
		this.userId = userId;
	}
}
