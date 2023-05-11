package com.vz.backend.business.dto.hstl;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.config.FolderTypeEnum;
import com.vz.backend.business.domain.hstl.HsFolder;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FolderTree {
	private Long id;
	private String title;
	private Long parentId;
	private Long headingsId;
	private List<FolderTree> children = new ArrayList<>();
	private FolderTypeEnum folderType = FolderTypeEnum.COQUAN;
	private String fileCode;
	@JsonIgnore
	private String fileNotation;
	
//	@JsonIgnore
	private String maintenance;
	
//	@JsonIgnore
	private String creator;
	
	@JsonIgnore
	private String description;
	
//	@JsonIgnore
	private String maintenanceCode;
	
	@JsonIgnore
	private FolderTree parent;
	
	@JsonIgnore
	private String article;
	
	/**
	 * Thời gian tài liệu
	 */
	@JsonIgnore
	private Date updateDate;
	
	/**
	 * Số tờ/ Số trang
	 */
	@JsonIgnore
	private Integer pageNumber;
	
	public FolderTree(HsFolder f) {
		super();
		this.id = f.getId();
		this.title = f.getTitle();
		this.parentId = f.getParentId();
		this.headingsId = f.getHeadingsId();
		this.fileNotation = convert(f.getFileNotation());
		this.title = convert(f.getTitle());
		this.maintenance = convert(f.getMainternanceStr());
		this.creator = convert(f.getCreator());
		this.description = convert(f.getDescription());
		this.fileCode = f.getFileCode();
		this.maintenanceCode = f.getMaintenanceObj() != null ? f.getMaintenanceObj().getCode() : null;
		this.updateDate = f.getUpdateDate();
		this.pageNumber = f.getPageNumber();
	}
	
	private String convert(String s) {
		return s == null ? "" : s;
	}
}
