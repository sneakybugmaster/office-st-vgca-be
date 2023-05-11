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
public class IconBasicDto {
	private Long id;
	private String name;
	private String fileType; // => file
	private DocumentTypeEnum docType; // => document
	private String downloadLink; // => file
	
	public IconBasicDto(Long id, String name) {
		super();
		this.id = id;
		this.name = name;
	}
	
	public IconBasicDto(Long id, DocumentTypeEnum docType) {
		super();
		this.id = id;
		this.docType = docType;
	}
	
	public IconBasicDto(Long id, String name, String fileType) {
		this(id, FilesStorageService.origin(name));
		this.fileType = fileType;
		this.downloadLink = "/hstl/download/" + PasswordUtils.signName(name);
	}

	public IconBasicDto(Long id, String nameIn, String nameOut, DocumentTypeEnum docType) {
		super();
		this.id = id;
		this.docType = docType;
		if (DocumentTypeEnum.VAN_BAN_DEN.equals(docType)) {
			this.name = nameIn;
		}
		if (DocumentTypeEnum.VAN_BAN_DI.equals(docType)) {
			this.name = nameOut;
		}
	}
}
