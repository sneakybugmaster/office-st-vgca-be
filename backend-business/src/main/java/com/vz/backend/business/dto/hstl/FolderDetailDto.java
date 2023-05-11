package com.vz.backend.business.dto.hstl;

import java.util.Date;

import com.vz.backend.business.config.FolderTypeEnum;
import com.vz.backend.business.config.IconTypeEnum;
import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.core.common.BussinessCommon;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FolderDetailDto {
	private Long id;
	private String title;
	private String fileCode;
	private Long totalDoc;
	private FolderTypeEnum folderType;
	private IconTypeEnum iconType;
	private Long maintenance;
	private String maintenanceObj;
	private Date createDate;
	private String createBy;
	private String userApprove;
	private Long userApproveId;
	private String orgQLName;
	private Long createById;
	private Long headingsId;
	private String identifier;
	private Long pageAmount;
	private FolderButton button;

	public FolderDetailDto(Long id, String name, String fileCode, FolderTypeEnum folderType) {
		super();
		this.id = id;
		this.title = name;
		this.fileCode = fileCode;
		this.folderType = folderType;
	}

	public FolderDetailDto(Long id, String name, String fileCode, Long totalDoc, FolderTypeEnum folderType) {
		this(id, name, fileCode, folderType);
		this.totalDoc = totalDoc;
	}

	public FolderDetailDto(Long id, String name, String fileCode, Long totalItems, Long thoiHanId, Date ngayTao, String nguoiTao) {
		this(id, name, fileCode, totalItems, null);
		this.maintenance = thoiHanId;
		this.createDate = ngayTao;
		this.createBy= nguoiTao;
	}
	
	public FolderDetailDto(Long id, String name, String fileCode, Long totalItems, Long thoiHanId, Date ngayTao, String nguoiTao, Long headingsId) {
		this(id, name, fileCode, totalItems, null);
		this.maintenance = thoiHanId;
		this.createDate = ngayTao;
		this.createBy= nguoiTao;
		this.headingsId = headingsId;
	}

	public FolderDetailDto(Long id, String name, String fileCode, FolderTypeEnum folderType, Long userId) {
		this(id, name, fileCode, folderType);
		setIconType(folderType, userId);
	}

	public FolderDetailDto(Long id, String name, String fileCode, Long totalItems, FolderTypeEnum folderType, Long userId) {
		this(id, name, fileCode, totalItems, folderType);
		setIconType(folderType, userId);
	}

	public FolderDetailDto(Long id, String name, String fileCode, Long totalItems, FolderTypeEnum folderType, Long userId, Long thoiHanId, Date ngayTao, String nguoiTao) {
		this(id, name, fileCode, totalItems, folderType, userId);
		this.maintenance = thoiHanId;
		this.createDate = ngayTao;
		this.createBy = nguoiTao;
	}

	public FolderDetailDto(HsFolder f, String nguoiTao, Long userApproveId, String nguoiDuyet, String thoiHan, String orgQLName) {
		this(f, nguoiTao, null);
		this.userApproveId = userApproveId;
		this.userApprove = nguoiDuyet;
		this.maintenanceObj = thoiHan;
		this.orgQLName = orgQLName;
	}
	
	public FolderDetailDto(HsFolder f, String createBy) {
		this.id = f.getId();
		this.title = f.getTitle();
		this.fileCode = f.getFileCode();
		this.totalDoc = f.getTotalDoc();
		this.folderType = f.getFolderType();
		this.maintenance = f.getMaintenance();
		this.createDate = f.getCreateDate();
		this.createBy = createBy;
		this.createById = f.getCreateBy();
		this.headingsId = f.getHeadingsId();
		this.pageAmount = f.getTotalDoc();
		this.identifier = f.getIdentifier();
		this.folderType = f.getFolderType();
		setIconType(f.getFolderType(), f.getCreateBy());
	}
	
	public FolderDetailDto(HsFolder f, String createBy, Long beShared) {
		this(f, createBy);
		this.button = new FolderButton(f.getStatus(), f.getParentId(), f.getTotalDoc(), f.getCreateBy(), beShared);
	}
	
	private void setIconType(FolderTypeEnum folderType, Long userId) {
		if (FolderTypeEnum.COQUAN.equals(folderType)) {
			this.iconType = IconTypeEnum.ORG;
		} else if (userId.equals(BussinessCommon.getUserId())) {
			this.iconType = IconTypeEnum.PERSONAL;
		} else {
			this.iconType = IconTypeEnum.SHARE;
		}
	}
}
