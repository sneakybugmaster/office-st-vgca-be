package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Entity
@Table(name = "CALENDAR_JOIN2", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class CalendarJoin2 extends BaseModel {
	private static final long serialVersionUID = 1L;
	@Column(name = "calendar_id", nullable = false)
	private Long calendarId;
	@JsonIgnore
	@ToString.Exclude
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "calendar_id", insertable = false, updatable = false)
	private Calendar2 calendar;

	@Column(name = "user_id")
	private Long userId;
//	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", insertable = false, updatable = false)
	private User user;
	
	@Column(name = "is_owner")
	private Boolean isOwner;
	
	@Column(name = "is_prepare")
	private Boolean isPrepare;
	
	@Column(name = "prepare_note")
	private String prepareNote;

	public void validCalendarJoin() {
		if (this.getUserId() == null) {
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
		}
		BussinessCommon.validLengthData(this.getPrepareNote(), "Nội dung chuẩn bị", 10000);
	}

	public void setCalendarJoin(CalendarJoin2 news) {
		this.setIsPrepare(news.getIsPrepare());
		this.setPrepareNote(news.getPrepareNote());
		this.setIsOwner(news.getIsOwner());
	}
}
