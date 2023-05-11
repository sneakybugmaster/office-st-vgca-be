package com.vz.backend.business.dto.hstl;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class FolderApprovedDto {
	
	public static final String CONSTRUCTOR = "new com.vz.backend.business.dto.hstl.FolderApprovedDto("
			+ "f.id, f.title, f.fileCode, f.totalDoc)";
	private Long id;
	private String title;
	private String fileCode;
	private Long totalDoc;
}
