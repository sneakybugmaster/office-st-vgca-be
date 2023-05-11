package com.vz.backend.business.dto.hstl.export;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Getter
public class ExportDocs extends ExportBase {
	private String fileNotation;
	private List<ContentDoc> contents;
	
	public ExportDocs(List<ContentDoc> content, String fileNotation) {
		super();
		this.contents = content;
		this.fileNotation = fileNotation == null ? "" : fileNotation;
	}
}
