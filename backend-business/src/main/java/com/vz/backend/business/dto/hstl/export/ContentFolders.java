package com.vz.backend.business.dto.hstl.export;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.domain.hstl.Headings;
import com.vz.backend.business.dto.hstl.FolderTree;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Getter
public class ContentFolders {
	/**
	 * Số thứ tự
	 */
	@Setter
	private int no;
	
	/**
	 * Số kí hiệu hồ sơ 
	 */
	@Setter
	private String fileNotation = "";
	
	/**
	 * Tiêu đề hồ sơ
	 */
	private String title = "";
	
	/**
	 * Thời hạn lưu trữ hồ sơ
	 */
	private String maintenance = "";
	
	/**
	 * Người tạo
	 */
	private String creator = "";
	
	/**
	 * ghi chú
	 */
	private String description = "";
	
	/**
	 * Thời gian tài liệu
	 */
	@JsonIgnore
	private String time = "";
	
	/**
	 * Số tờ/ Số trang
	 */
	@JsonIgnore
	private int pageNumber = 0;
	
	
	@JsonIgnore
	private Boolean limitation = null;

	public ContentFolders(String headings, FolderTree f) {
		if (f == null) {
			this.title = headings;
			return;
		}

		this.fileNotation = BussinessCommon.toString(f.getFileNotation());
		this.title = BussinessCommon.toString(f.getTitle());
		this.maintenance = BussinessCommon.toString(f.getMaintenance());
		this.creator = BussinessCommon.toString(f.getCreator());
		this.description = BussinessCommon.toString(f.getDescription());
		this.limitation = !Constant.THVV.equals(f.getMaintenanceCode());
		this.pageNumber = f.getPageNumber() == null ? 0 : f.getPageNumber();
		this.time = DateTimeUtils.formatDate(f.getUpdateDate(), "dd/MM/yyyy");
	}

	public static List<ContentFolders> convert(List<Headings> headings, List<FolderTree> folders) {
		List<ContentFolders> rs = new ArrayList<>();
		ContentFolders tmp = null;
		for (Headings i : headings) {
			tmp = new ContentFolders(i.getName(), null);
			rs.add(tmp);
			for (FolderTree f : folders) {
				if (i.getId().equals(f.getHeadingsId())) {
					tmp = new ContentFolders(i.getName(), f);
					rs.add(tmp);
				}
			}
		}

		setNumber(rs, true);
		return rs;
	}
	
	public static List<ContentFolders> convert(List<FolderTree> folders) {
		List<ContentFolders> rs = new ArrayList<>();
		ContentFolders tmp = null;
		for (FolderTree f : folders) {
			tmp = new ContentFolders(null, f);
			rs.add(tmp);
		}

		setNumber(rs, false);
		return rs;
	}

	/*
	 * Danh mục hồ sơ (cây đề mục) addNo = true
	 * Phiếu mục lục hồ sơ, tài liệu nộp lưu, addNo = false
	 */
	private static void setNumber(List<ContentFolders> rs, boolean addNo) {
		int no = 1;
		for (int i = 0; i < rs.size(); i++) {
			ContentFolders r = rs.get(i);
			r.setNo(no);
			if(addNo) {
				r.setFileNotation(no + ". " + r.getFileNotation());
			}
			no++;
		}
	}
}
