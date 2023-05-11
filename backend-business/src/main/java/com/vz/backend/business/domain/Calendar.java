package com.vz.backend.business.domain;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Category;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "CALENDAR", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Calendar extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "title")
	private String title;
	@Column(name = "address")
	private String address;
	@Column(name = "org_id")
	private Long orgId;
	@Column(name = "type")
	private Long type;
	@OneToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "type", insertable = false, updatable = false)
	private Category catType;
	@Column(name = "room_id")
	private Long roomId;
	@OneToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "room_id", insertable = false, updatable = false)
	private Category room;
	@Column(name = "description")
	private String description;
	@Column(name = "start_time")
	@JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm", timezone = "Asia/Ho_Chi_Minh")
	private Date startTime;
	@Column(name = "end_time")
	@JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm", timezone = "Asia/Ho_Chi_Minh")
	private Date endTime;
	@Column(name = "schedure_type")
	private Long schedureType;
	@Column(name = "book_by")
	private Long bookBy;
	@Column(name = "contact_info")
	private String contactInfo;
	@Column(name = "status")
	private Integer status;

}
