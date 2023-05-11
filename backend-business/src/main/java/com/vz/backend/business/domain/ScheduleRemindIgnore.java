package com.vz.backend.business.domain;

import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
@Entity
@Table(name = "ScheduleRemindIgnore", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(name ="user_remind", columnNames = { "userId", "remindId" }) })
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class ScheduleRemindIgnore extends BaseModel {
	private static final long serialVersionUID = 1L;
	private Long userId;
	private Long remindId;
}
