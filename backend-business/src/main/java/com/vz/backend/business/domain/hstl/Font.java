package com.vz.backend.business.domain.hstl;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.domain.standard.BaseCode;
import com.vz.backend.core.common.BussinessCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "FONT", schema = "vz", uniqueConstraints = { 
		@UniqueConstraint(name = "fond_name_uq", columnNames = { "client_id", "org_id", "fond_name" }),
		@UniqueConstraint(name = "organ_ld_uq", columnNames = { "client_id", "org_id", "organ_ld" })
		})
@NoArgsConstructor
@AllArgsConstructor
@Setter
@Getter
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "clientId", "active" })
public class Font extends BaseCode {

	/**
	 * length: 200
	 * Tên phông/công trình/sưu tập lưu trữ
	 */
	@Column(name = "fond_name")
	private String fondName;
	
	/**
	 * LongText
	 * Lịch sử đơn vị hình thành phông
	 */
	@Column(columnDefinition = "TEXT", name = "fond_history")
	private String fondHistory;
	
	/**
	 * length: 30
	 * Thời gian tài liệu
	 */
	private String archivesTime;
	
	/**
	 * length: 10
	 * Tổng số tài liệu giấy
	 */
	private Integer paperTotal;
	
	/**
	 * length: 10
	 * Số lượng tài liệu giấy đã số hóa
	 */
	private Integer paperDigital;
	
	/**
	 * length : 300
	 * Các nhóm tài liệu chủ yếu
	 */
	@Column(columnDefinition = "TEXT", name = "key_groups")
	private String keyGroups;
	
	/**
	 * lenght: 300
	 * Các loại hình tài liệu khác
	 */
	@Column(columnDefinition = "TEXT", name = "other_types")
	private String otherTypes;
	
	/**
	 * lenght: 100
	 * Ngôn ngữ
	 */
	private String language;
	
	/**
	 * lenght: 50
	 * Công cụ tra cứu
	 */
	private String lookupTools;
	
	/**
	 * lenght: 10
	 * Số lượng trang tài liệu đã lập bản sao bảo hiểm
	 * 
	 */
	private Integer copyNumber;
	
	/**
	 * lenght: 1000
	 * Ghi chú
	 */
	@Column(columnDefinition = "TEXT", name = "description")
	private String description;
	
	@Column(name = "org_id")
	private Long orgId;
	
	@Override
	public void valids() {
		super.valids();
		BussinessCommon.require("Tên phông/công trình/sưu tập lưu trữ", this.fondName);
		BussinessCommon.validLengthData(this.fondName, "Tên phông/công trình/sưu tập lưu trữ", 200);
		BussinessCommon.validLengthData(this.fondHistory, "Lịch sử đơn vị hình thành phông", 1000);
		BussinessCommon.validLengthData(this.description, "Ghi chú", 1000);
		BussinessCommon.validLengthData(this.otherTypes, "Các loại hình tài liệu khác", 300);
		BussinessCommon.validLengthData(this.lookupTools, "Công cụ tra cứu", 50);
	}
	
	public void set(Font f) {
		this.fondName = f.fondName;
		this.fondHistory = f.fondHistory;
		this.archivesTime = f.archivesTime;
		this.paperTotal = f.paperTotal;
		this.paperDigital = f.paperDigital;
		this.keyGroups = f.keyGroups;
		this.otherTypes = f.otherTypes;
		this.language = f.language;
		this.lookupTools = f.lookupTools;
		this.copyNumber = f.copyNumber;
		this.description = f.description;
	}
	
	@PrePersist
	@PreUpdate
	public void prePersit() {
		this.orgId = BussinessCommon.getOrgId();
	}
	
	@Override
	public String getIdentifier() {
		return super.getIdentifier() == null ? "" : super.getIdentifier();
	}
}
