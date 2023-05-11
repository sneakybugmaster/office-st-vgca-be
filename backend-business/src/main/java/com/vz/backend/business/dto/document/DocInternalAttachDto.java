package com.vz.backend.business.dto.document;

import com.vz.backend.core.config.AttachmentTypeEnum;
import com.vz.backend.core.service.FilesStorageService;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class DocInternalAttachDto {
	private Long id;
	private String name;
	private String type;
	private Long size;
	private AttachmentTypeEnum attachType;
	private Boolean encrypt;
	
	public String getDisplayName() {
		return FilesStorageService.origin(this.name);
	}

	public DocInternalAttachDto(Long id, String name, String type, Long size, AttachmentTypeEnum attachType,
			Boolean encrypt) {
		super();
		this.id = id;
		this.name = name;
		this.type = type;
		this.size = size;
		this.attachType = attachType;
		this.encrypt = encrypt == null ? Boolean.FALSE : encrypt;
	}
}
