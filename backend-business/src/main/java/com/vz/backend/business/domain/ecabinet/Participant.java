package com.vz.backend.business.domain.ecabinet;

import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.config.ReceiveTypeEnum;
import com.vz.backend.business.config.ecabinet.RoleEnum;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;

import lombok.Data;

/**
 * Thành phần tham gia
 *
 */
@Entity
@Table(name = "PARTICIPANT", schema = "vz")
@Data
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
public class Participant extends BaseModel {

	private static Participant instance;

	private Participant() {
	}

	public static Participant getInstance() {
		if (instance == null) {
			instance = new Participant();
		}
		return instance;
	}

	/**
	 * Thông tin cá nhân/tổ chức/nhóm cá nhân
	 */
	@Column(name = "obj_id")
	private Long objId;

	/**
	 * Loại thành viên: cá nhân/tổ chức/nhóm cá nhân
	 */
	@Enumerated(EnumType.STRING)
	@Column(name = "type")
	private ReceiveTypeEnum type;

	/**
	 * Vai trò: Chủ trì/ thành viên/ khách mời
	 */
	@Enumerated(EnumType.STRING)
	@Column(name = "role")
	private RoleEnum role;

	@Column(name = "meeting_id")
	private Long meetingId;
	@JsonIgnore
	@JoinColumn(name = "meeting_id", insertable = false, updatable = false)
	@ManyToOne(fetch = FetchType.LAZY)
	private Meeting meeting;

	/*
	 * Nhóm thành viên
	 */
	private Long groupId;

	@Override
	public void valids() {
		String label = "Cá nhân/ tổ chức tham dự";
		BussinessCommon.require(label, this.objId);
		BussinessCommon.require(label, this.role);
		BussinessCommon.require(label, this.type);
		if (ReceiveTypeEnum.GROUP.equals(this.type)) {
			BussinessCommon.require("Nhóm thành viên", this.groupId);
		}
	}

	public void setObjId(List<Participant> participants, Long objId) {
		if (BussinessCommon.isEmptyList(participants))
			return;
		participants.forEach(i -> i.setMeetingId(objId));
	}

	public void set(Participant participant) {
		this.objId = participant.getObjId();
		this.type = participant.getType();
		this.role = participant.getRole();
		this.meetingId = participant.getMeetingId();
	}

	public Participant(Long objId, ReceiveTypeEnum type, RoleEnum role) {
		super();
		this.objId = objId;
		this.type = type;
		this.role = role;
	}
	
	public Participant(Long objId, ReceiveTypeEnum type, RoleEnum role, Long meetingId, Long groupId) {
		super();
		this.objId = objId;
		this.type = type;
		this.role = role;
		this.meetingId = meetingId;
		this.groupId = groupId;
	}

}
