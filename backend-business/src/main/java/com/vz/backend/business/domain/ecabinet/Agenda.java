package com.vz.backend.business.domain.ecabinet;

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

import org.springframework.web.client.RestClientException;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.config.ecabinet.MeetingStatusEnum;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.DateTimeUtils;

import lombok.Data;

/**
 * Nội dung họp
 *
 */
@Entity
@Table(name = "AGENDA", schema = "vz")
@Data
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
public class Agenda extends BaseModel {

	private static Agenda instance;

	public Agenda() {
	}

	public static Agenda getInstance() {
		if (instance == null) {
			instance = new Agenda();
		}
		return instance;
	}

	/**
	 * Thời gian bắt đầu
	 */
	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY_HH_MM_SS, timezone = "Asia/Ho_Chi_Minh")
	@Column(name = "start_date")
	private Date start;

	/**
	 * Thời gian kết thúc
	 */
	@JsonFormat(pattern = DateTimeUtils.DD_MM_YYYY_HH_MM_SS, timezone = "Asia/Ho_Chi_Minh")
	@Column(name = "end_date")
	private Date end;

	/**
	 * Nội dung
	 */
	@Column(name = "content", columnDefinition = "TEXT")
	private String content;

	/**
	 * Cơ quan chủ trì
	 */
	@Column(name = "host_id")
	private Long hostId;
	@JsonIgnore
	@JoinColumn(name = "host_id", insertable = false, updatable = false)
	@ManyToOne(fetch = FetchType.LAZY)
	private Organization host;

	/**
	 * Chuyên viên phụ trách
	 */
	@Column(name = "expert_id")
	private Long expertId;
	@JsonIgnore
	@JoinColumn(name = "expert_id", insertable = false, updatable = false)
	@ManyToOne(fetch = FetchType.LAZY)
	private User expert;

	/**
	 * Vấn đề bổ sung
	 * 
	 */
	@OneToMany(mappedBy = "agendaId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	private List<Problem> problems;

	@Column(name = "meeting_id")
	private Long meetingId;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "meeting_id", updatable = false, insertable = false)
	@JsonIgnore
	private Meeting meeting;
	/**
	 * Tài liệu đính kèm
	 */
	@Transient
	private List<AttachmentMeeting> attachments;

	/**
	 * Ẩn nội dung
	 * 
	 */
	@Column(name = "hiden")
	private Boolean hiden;

	/**
	 * Trạng thái cuộc họp
	 * 
	 */
	@Column(name = "status")
	@Enumerated(EnumType.STRING)
	private MeetingStatusEnum status;

	@Override
	public void valids() {
		BussinessCommon.require("Nội dung họp", this.content);
		BussinessCommon.require("Cơ quan chủ trì", this.hostId);
		BussinessCommon.require("Chuyên viên phụ trách", this.expertId);
//		BussinessCommon.require("Thời gian bắt đầu", this.start);
//		BussinessCommon.require("Thời gian kết thúc", this.end);
		BussinessCommon.validLengthData(this.content, "Nội dung", 2000);

		if (this.end != null && this.start != null) {
			if (this.end.getTime() < this.start.getTime()) {
				throw new RestExceptionHandler("Thời gian kết thúc phải sau thời gian bắt đầu");
			}
		}

		if (!BussinessCommon.isEmptyList(this.problems)) {
			this.problems.forEach(Problem::valids);
		}
	}

	public void setObjId(List<Agenda> agendas, Long objId) {
		if (BussinessCommon.isEmptyList(agendas))
			return;
		agendas.forEach(i -> i.setMeetingId(objId));
	}

	public void set(Agenda agenda) {
		this.start = agenda.getStart();
		this.end = agenda.getEnd();
		this.content = agenda.getContent();
		this.hostId = agenda.getHostId();
		this.expertId = agenda.getExpertId();
		this.status = agenda.getStatus();
		this.hiden = agenda.getHiden();
		
	}

	public Agenda(Date start, Date end, String content, Long hostId, Long expertId) {
		super();
		this.start = start;
		this.end = end;
		this.content = content;
		this.hostId = hostId;
		this.expertId = expertId;
	}

	@PrePersist
	public void prePersit() {
		if (this.hiden == null) {
			this.hiden = false;
		}
	}
}
