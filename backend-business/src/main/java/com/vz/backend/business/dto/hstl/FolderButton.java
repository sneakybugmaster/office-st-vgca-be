package com.vz.backend.business.dto.hstl;

import java.util.Arrays;
import java.util.List;

import com.vz.backend.business.config.HsFolderStatusEnum;
import com.vz.backend.core.common.BussinessCommon;

import lombok.Getter;

@Getter
public class FolderButton {

	public enum TypeFolder {
		BE_SHARED("Được chia sẻ"), SHARED("Chia sẻ"), CREATE("Mới tạo"), MOVE_HSVC("Chuyển sang hồ sơ công việc");

		private String name;

		private TypeFolder(String name) {
			this.name = name;
		}
	}

	/**
	 * determind type folder
	 */
	private TypeFolder typeFolder;

	/**
	 * btn Đổi tên
	 */
	private boolean edit;

	/**
	 * btn xóa
	 */
	private boolean del;

	/**
	 * btn chia sẻ
	 */
	private boolean share;

	/**
	 * di chuyển sang hồ sơ cá nhân
	 */
	private boolean move2HSCN;

	/**
	 * di chuyển sang hồ sơ công việc
	 */
	private boolean move2HSCV;

	/**
	 * btn thêm tài liệu
	 */
	private boolean addDoc;
	
	/**
	 * btn dừng chia sẻ
	 */
	private boolean stopShare;
	
	/**
	 * 
	 * @param status
	 * @param parentId
	 * @param totalDoc
	 * @param createBy
	 * @param beShared
	 */
	public FolderButton(HsFolderStatusEnum status, Long parentId, Long totalDoc, Long createBy, Long beShared) {
		if (beShared != null && beShared.equals(createBy)) {
			beShared = null;
		}
		boolean hsTaiLieu = HsFolderStatusEnum.HS_TAI_LIEU.equals(status);
		boolean hsCongViec = HsFolderStatusEnum.HS_CONG_VIEC.equals(status) || HsFolderStatusEnum.HS_TRA_LAI.equals(status);
		List<HsFolderStatusEnum> openingFolders = Arrays.asList(HsFolderStatusEnum.HS_TAI_LIEU,
				HsFolderStatusEnum.HS_CONG_VIEC, HsFolderStatusEnum.PB_CHO_DUYET);
		this.edit = hsTaiLieu;
		this.del = hsTaiLieu || hsCongViec;
		this.share = hsTaiLieu;
		this.move2HSCN = hsTaiLieu;
		this.move2HSCV = hsTaiLieu && parentId == null && totalDoc != null && totalDoc.longValue() > 0;
		this.addDoc = openingFolders.contains(status);
		this.setTypeFolder(status, createBy, beShared);
		this.stopShare = TypeFolder.SHARED.equals(this.typeFolder);
	}

	public void setTypeFolder(HsFolderStatusEnum status, Long createBy, Long beShared) {
		boolean isCreateBy = BussinessCommon.getUserId().equals(createBy);
		boolean hsTailieu = HsFolderStatusEnum.HS_TAI_LIEU.equals(status);
		if (isCreateBy && hsTailieu && (beShared == null)) {
			this.typeFolder = TypeFolder.CREATE;
		} else if (!isCreateBy && hsTailieu && beShared != null) {
			this.typeFolder = TypeFolder.BE_SHARED;
		} else if (isCreateBy && hsTailieu && (beShared != null)) {
			this.typeFolder = TypeFolder.SHARED;
		} else {
			this.typeFolder = TypeFolder.MOVE_HSVC;
		}
	}
}
