package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "CALENDAR_JOIN", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class CalendarJoin extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "calendar_id")
	private Long calendarId;
	@Column(name = "org_id")
	private Long orgId;
	@Column(name = "user_id")
	private Long userId;
	@OneToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "user_id", insertable = false, updatable = false)
	private User user;
	@Column(name = "is_owner")
	private Boolean isOwner;
	@Column(name = "is_know")
	private Boolean isKnow;
	@Column(name = "is_prepare")
	private Boolean isPrepare;
	@Column(name = "prepare_note")
	private String prepareNote;

}
