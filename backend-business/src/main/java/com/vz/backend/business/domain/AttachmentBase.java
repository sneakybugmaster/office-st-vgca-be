package com.vz.backend.business.domain;

import javax.persistence.MappedSuperclass;

import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.service.FilesStorageService;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@MappedSuperclass
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class AttachmentBase extends BaseModel {
	private static final long serialVersionUID = 1L;

	private String name;
	private String type;
	private Long size;
	private Boolean encrypt;

	public String getDisplayName() {
		return FilesStorageService.origin(this.name);
	}

	public AttachmentBase(MultipartFile file) {
		this.type = file.getContentType();
		this.size = file.getSize();
	}

	public AttachmentBase(String name, String type, Long size) {
		this.name = name;
		this.type = type;
		this.size = size;
	}
	
	public boolean getEncrypt() {
		return this.encrypt == null ? Boolean.FALSE : this.encrypt;
	}
}
