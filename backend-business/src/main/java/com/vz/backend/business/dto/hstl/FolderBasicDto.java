package com.vz.backend.business.dto.hstl;

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
public class FolderBasicDto {
	private Long id;
	private String name;
	private String fileCode;
	private Long totalItems;
	private Long parentId;
	private IconTypeEnum iconType;
	private FolderButton button;
	
	public FolderBasicDto(Long id, String name, String fileCode) {
		super();
		this.id = id;
		this.name = name;
		this.fileCode = fileCode;
	}
	
	public FolderBasicDto(Long id, String name, String fileCode, Long totalItems) {
		this(id, name, fileCode);
		this.totalItems = totalItems;
	}
	
	public FolderBasicDto(Long id, Long parentId, String name, String fileCode) {
		this(id, name, fileCode);
		this.parentId = parentId;
	}
	
	public FolderBasicDto(Long id, String name, String fileCode, FolderTypeEnum folderType, Long userId, Long parentId) {
		this(id, name, fileCode);
		this.parentId = parentId;
		this.setIconType(folderType, userId);
	}
	
	public FolderBasicDto(Long id, String name, String fileCode, Long totalItems, FolderTypeEnum folderType, Long userId) {
		this(id, name, fileCode, totalItems);
		this.setIconType(folderType, userId);
	}
	
	private void setIconType(FolderTypeEnum folderType, Long userId) {
		if (FolderTypeEnum.COQUAN.equals(folderType)) {
			this.iconType = IconTypeEnum.ORG;
		} else {
			if (userId.equals(BussinessCommon.getUserId()))
				this.iconType = IconTypeEnum.PERSONAL;
			else
				this.iconType = IconTypeEnum.SHARE;
		}
	}
	
	public FolderBasicDto(HsFolder f, Long beShared) {
		this.id = f.getId();
		this.name = f.getTitle();
		this.fileCode = f.getFileCode();
		this.totalItems = f.getTotalDoc();
		this.parentId = f.getParentId();
		this.setIconType(f.getFolderType(), f.getCreateBy());
		this.button = new FolderButton(f.getStatus(), f.getParentId(), f.getTotalDoc(), f.getCreateBy(), beShared);
	}
}
