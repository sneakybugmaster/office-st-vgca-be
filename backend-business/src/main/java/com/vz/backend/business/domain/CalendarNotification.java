package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.PrePersist;
import javax.persistence.Table;

import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "CALENDAR_NOTIFICAITON", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class CalendarNotification extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "user_id")
	private Long userId;

	@Column(name = "calendar_id")
	private Long calendarId;

	@Column(name = "description")
	private String description; // prepare_note

	@Column(name = "[read]") // Trạng thái xem
	private Boolean read;

	@Enumerated(EnumType.STRING)
	@Column(name = "module_code")
	private ModuleCodeEnum moduleCode;

	@Column(name = "role")
	private Boolean role; // true : chủ trì, false : chuẩn bị

	@PrePersist
	public void prePersist() {
		this.read = false;
	}
}
