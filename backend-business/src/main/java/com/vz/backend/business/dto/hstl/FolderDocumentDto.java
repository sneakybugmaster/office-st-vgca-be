package com.vz.backend.business.dto.hstl;

import com.vz.backend.business.config.IconTypeEnum;
import com.vz.backend.business.domain.hstl.HsFolderDocument;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FolderDocumentDto {

	private HsFolderDocument doc;
	private IconTypeEnum iconType;
	public FolderDocumentDto(HsFolderDocument doc) {
		super();
		this.iconType = IconTypeEnum.DOC;
		this.doc = doc;
	}
	
}
