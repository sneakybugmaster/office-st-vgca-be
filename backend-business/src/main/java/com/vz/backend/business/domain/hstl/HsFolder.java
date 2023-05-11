package com.vz.backend.business.domain.hstl;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.regex.Pattern;

import javax.persistence.Column;
import javax.persistence.ColumnResult;
import javax.persistence.ConstructorResult;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrePersist;
import javax.persistence.SqlResultSetMapping;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.vz.backend.util.StringUtils;
import org.hibernate.annotations.NamedNativeQueries;
import org.hibernate.annotations.NamedNativeQuery;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.config.FolderTypeEnum;
import com.vz.backend.business.config.HsFolderStatusEnum;
import com.vz.backend.business.domain.standard.BaseCode;
import com.vz.backend.business.util.Constant;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@SqlResultSetMapping(name = "folderUnionResult", classes = {
		@ConstructorResult(targetClass = com.vz.backend.business.dto.hstl.ReportProcessDto.class, columns = {
				@ColumnResult(name = "fId"), @ColumnResult(name = "fromUserId"), @ColumnResult(name = "toUserId"),
				@ColumnResult(name = "toOrgId"), @ColumnResult(name = "status"), @ColumnResult(name = "type") }) })
@NamedNativeQueries(value = {
		@NamedNativeQuery(name = "HsFolder.menu", query = Constant.FOLDER_MENU_QUERY, resultSetMapping = "folderUnionResult"), })
@Table(name = "HS_FOLDER", schema = "vz")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class HsFolder extends BaseCode {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	/**
	 * số kí hiệu hồ sơ
	 */
	@Column(name = "file_notation")
	private String fileNotation;
	/**
	 * 
	 * Năm hình thành hồ sơ
	 */
	@Column(name = "file_catalog")
	private String fileCatalog;

	@Column(name = "title")
	private String title; //tiêu đề hồ sơ
	@Column(name = "parent_id")
	private Long parentId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "parent_id", insertable = false, updatable = false)
	private HsFolder parent;
	@Column(name = "total_doc")
	private Long totalDoc; //tổng số văn bản trong hồ sơ

	@Column(name = "maintenance")
	private Long maintenance;//thời hạn bảo quản
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "maintenance", insertable = false, updatable = false)
	private Category maintenanceObj;

	@Column(name = "file_code")
	private String fileCode; //mã hồ sơ
	@Enumerated(EnumType.STRING)
	@Column(name = "folder_type")
	private FolderTypeEnum folderType;
	@Enumerated(EnumType.STRING)
	@Column(name = "status")
	private HsFolderStatusEnum status;
	@Column(name = "org_ql_id")
	private Long orgQLId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "org_ql_id", insertable = false, updatable = false)
	private Organization orgQL;
	/**
	 * //Chế độ sử dụng
	 */
	@Column(name = "rights")
	private String rights;

	@Column(name = "language")
	private String language;
	/**
	 * Name : Thời gian kết thúc
	 * Format : DD/MM/YYYY
	 */
	@Column(name = "start_date")
	private Date startDate;
	/**
	 * Name : Thời gian bắt đầu
	 * Format : DD/MM/YYYY
	 */
	@Column(name = "end_date") 
	private Date endDate;
	/**
	 * Name : Chú giải
	 * Maxlength : 2000
	 */
	@Column(name = "description", columnDefinition = "TEXT")
	private String description;
	/**
	 * Name: kí hiệu thông tin
	 */
	@Column(name = "infor_sign")
	private String inforSign;
	/**
	 * Name : từ khóa tìm kiếm
	 */
	@Column(name = "keyword")
	private String keyword; 
	@Column(name = "sheet_num")
	private int sheerNumber; // số tờ
	@Column(name = "page_num")
	private Integer pageNumber;//số trang
	
	/**
	 * Name : trạng thái vật lý
	 */
	@Column(name = "format")
	private String format;
	
	private Long headingsId;
	@JsonIgnore
	@JsonIgnoreProperties({ "hibernateLazyInitializer" })
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "headingsId", insertable = false, updatable = false)
	private Headings headingsObj;
	
	@JsonIgnore
	@JsonIgnoreProperties({ "hibernateLazyInitializer" })
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "create_by", insertable = false, updatable = false)
	private User createByObj;

	@Transient
	private List<HsFolder> children = new ArrayList<>();
	
	@Transient
	@JsonIgnore
	private String article;
	
	private Integer year;
	
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "update_by", insertable = false, updatable = false)
	private User updater;

	@PrePersist
	public void prePersist() {
		this.totalDoc = 0L;
		if (this.status == null) {
			this.status = HsFolderStatusEnum.HS_TAI_LIEU;
		}

		if (this.year == null) {
			Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
			this.year = c.get(Calendar.YEAR);
		}
	}

	@Override
	public void valids() {
		BussinessCommon.require("Tiêu đề hồ sơ", this.title);
		BussinessCommon.require("Mã hồ sơ", this.getFileCode());
		if(!BussinessCommon.isFileCode(this.getFileCode())) {
			throw new RestExceptionHandler(Message.INVALID_FILE_CODE);
		}
//		BussinessCommon.require("Năm hình thành hồ sơ", this.fileCatalog);
		BussinessCommon.require("Số và ký hiệu hồ sơ "
				, this.getFileNotation());
		//		if(!BussinessCommon.isFileNotation(this.fileCatalog))
		//			throw new RestExceptionHandler("Năm hình thành hồ sơ có định dạng : 0000");


		if(!BussinessCommon.isFileNotation(this.getFileNotation())) {
			throw new RestExceptionHandler("Số và ký hiệu hồ sơ phải có định dạng : 01.TH");
		}

		BussinessCommon.require("Thời hạn bảo quản", this.maintenance == null || this.maintenance == 0L ? null : this.maintenance.toString());
//		BussinessCommon.require("Chế độ sử dụng", this.rights);
//		BussinessCommon.require("Ngôn ngữ", this.language);
//		BussinessCommon.require("Thời gian bắt đầu", this.startDate == null ? null : this.startDate.toString());
//		BussinessCommon.require("Thời gian kết thúc", this.endDate == null ? null : this.endDate.toString());
//		BussinessCommon.require("Tổng số văn bản trong hồ sơ", this.totalDoc == null ? null : this.totalDoc.toString());
//		require("Chú giải", this.description);
		BussinessCommon.require("Đề mục", this.getHeadingsId());
		if(this.startDate != null && this.endDate != null && this.startDate.compareTo(endDate) > 0) {
			throw new RestExceptionHandler(Message.WRONG_CONDITION_DATE);
		}
	}
	
	public void set(HsFolder hs) {
		this.title = hs.title;
		this.description = hs.description;
		this.fileCode = hs.fileCode;
		this.fileNotation = hs.fileNotation;
		this.startDate = hs.startDate;
		this.folderType = hs.folderType;
		this.maintenance = hs.maintenance;
		this.keyword = hs.keyword;
		this.format = hs.format;
		this.language = hs.language;
		this.inforSign = hs.inforSign;
		this.endDate = hs.endDate;
		this.pageNumber = hs.pageNumber;
		this.sheerNumber = hs.sheerNumber;
		this.rights = hs.rights;
		this.headingsId = hs.headingsId;
	}
	
	public String getCreator() {
		return this.getCreateBy() != null ? this.getCreateByObj().getFullName() : "";
	}
	
	public String getMainternanceStr() {
		return this.getMaintenanceObj() != null ?  this.getMaintenanceObj().getName() : "";
	}

	// Using the 'super' key instead of using 'this' helps you avoid Infinite recursion error
	@Override
	public String getIdentifier() {
		return super.getIdentifier() == null ? "" : super.getIdentifier();
	}

	@Override
	public String getOrganld() {
		return super.getOrganld() == null ? "" : super.getOrganld();
	}
}
