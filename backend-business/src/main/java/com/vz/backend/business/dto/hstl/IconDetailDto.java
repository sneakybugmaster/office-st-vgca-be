package com.vz.backend.business.dto.hstl;

import java.util.Date;

import com.vz.backend.business.config.IconTypeEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.PasswordUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class IconDetailDto {
	private Long id;
	private Long docId;
	private String name;
	private String fileType; // => file
	private IconTypeEnum iconType;
	private DocumentTypeEnum docType; // => document
	private Date ngayTao;
	private String soKyHieu;
	private String downloadLink;
//	private String noiGui;

	public IconDetailDto(Long id, String name, Date ngayTao, String fileType) {
		this(id, FilesStorageService.origin(name), ngayTao, fileType, IconTypeEnum.FILE, null);
		this.downloadLink = "/hstl/download/" + PasswordUtils.signName(name);
	}

	public IconDetailDto(Long id, Long docId, String nameIn, String nameOut, String soKyHieuDen, String soKyHieuDi, Date ngayTao, DocumentTypeEnum docType) {
		this(id, null, ngayTao, null, IconTypeEnum.DOC, docType);
		this.docId = docId;
		if (DocumentTypeEnum.VAN_BAN_DEN.equals(docType)) {
			this.name = nameIn;
			this.soKyHieu = soKyHieuDen;
//			this.noiGui = noiGui;
		}
		if (DocumentTypeEnum.VAN_BAN_DI.equals(docType)) {
			this.name = nameOut;
			this.soKyHieu = soKyHieuDi;
//			this.noiGui = noiGui;
		}
	}

	public IconDetailDto(Long id, String name, Date ngayTao, String fileType, IconTypeEnum iconType, DocumentTypeEnum docType) {
		super();
		this.id = id;
		this.name = name;
		this.ngayTao = ngayTao;
		this.fileType = fileType;
		this.iconType = iconType;
		this.docType = docType;
	}
}
