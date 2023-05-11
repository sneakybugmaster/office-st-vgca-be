package com.vz.backend.business.dto.hstl;

import java.util.Date;

import com.vz.backend.business.config.FolderTypeEnum;
import com.vz.backend.business.config.IconTypeEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FolderDetailBasicDto {
	private Long id;
	private String name;
	//private Long totalItems;
	private FolderTypeEnum folderType;
	private String thoiHan;
	private Date ngayTao;
	private String nguoiTao;
	//private String nguoiDuyet;
	private String orgQLName;
	
	public FolderDetailBasicDto(Long id, String name, FolderTypeEnum folderType, String thoiHan, Date ngayTao,
			String nguoiTao, String orgQLName) {
		super();
		this.id = id;
		this.name = name;
		this.folderType = folderType;
		this.thoiHan = thoiHan;
		this.ngayTao = ngayTao;
		this.nguoiTao = nguoiTao;
		this.orgQLName = orgQLName;
	}
}
