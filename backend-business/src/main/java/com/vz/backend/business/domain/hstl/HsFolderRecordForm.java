package com.vz.backend.business.domain.hstl;

import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.config.HsRecordEnum;
import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "HS_FOLDER_RECORD_FORM", schema = "vz")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class HsFolderRecordForm extends BaseModel {
	
	private String name;
	/**
	 * Trạng thái phiếu
	 */
	private Integer status; // 1: Gửi mới, 2: chờ duyệt 3: Bổ sung, 4: từ chối, 5: duyệt

	/** 
	 * Tổ chức đề nghị 
	 */
	@Column(name = "org_id")
	private Long orgId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "org_id", insertable = false, updatable = false)
	private Organization org;
	/**
	 * Nguồn nộp lưu
	 */
	@Enumerated(EnumType.STRING)
	@Column(nullable = false)
	private HsRecordEnum src;

	/**
	 * Số lần nộp lưu
	 */
	private String times;

	/**
	 * Tổng số hồ sơ
	 */
	private Integer total;

	/**
	 * Tổng số trang
	 */
	private Integer totalPage;

	/**
	 * Tổng số dung lượng
	 */
	private Integer size;

	/**
	 * Cách thức nộp lưu
	 */
	@Enumerated(EnumType.STRING)
	private HsRecordEnum way;

	/**
	 * Thời gian dự kiến nộp
	 */
	private Date date;

	/**
	 * Địa chỉ liên hệ
	 */
	private String address;

	/**
	 * Ghi chú
	 */
	private String note;
	
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "create_by", insertable = false, updatable = false)
	private User creator;

	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "update_by", insertable = false, updatable = false)
	private User updater;
	
	// save ECM info
	private Date dateResponse;
	private String userResponse;
	private String noteResponse;
	
	@Transient
	private List<TaskAttachment> attachments;
	
	@Override
	public void valids() {
		BussinessCommon.require("Nguồn nộp lưu", this.src);
		BussinessCommon.require("Tổng số hồ sơ", this.total);
		BussinessCommon.require("Nội dung phiếu", this.name);
		BussinessCommon.require("Cách thức nộp lưu", this.way);
		BussinessCommon.require("Thời gian dự kiến nộp", this.date);
		BussinessCommon.validLengthData(this.name, "Nội dung phiếu", 250);
	}

	@PrePersist
	public void prePersit() {
		this.status = 1;
	}
	
	public void set(HsFolderRecordForm input) {
		this.name = input.getName();
		this.src = input.getSrc();
		this.size = input.getSize();
		this.total = input.getTotal();
		this.totalPage = input.getTotalPage();
		this.note = input.getNote();
		this.address = input.getAddress();
		this.way = input.getWay();
		this.times = input.getTimes();
	}
	
	public String getStatusStr() {
		String rs = "";
		switch (this.status) {
		case 1:
			rs = "Tạo mới";
			break;
		case 2:
			rs = "Chờ duyệt";
			break;
		case 3:
			rs = "Bổ sung";
			break;
		case 4:
			rs = "Từ chối";
			break;
		case 5:
			rs = "Duyệt";
			break;
		default:
			break;
		}
		return rs;
	}
	
	public String getCreatorName() {
		return this.creator == null ? "" : this.creator.getFullName();
	}
	
	public String getWayName() {
		return this.way == null ? "" : this.way.name;
	}
	
	public String getSrcName() {
		return this.src == null ? "" : this.src.name;
	}
}
