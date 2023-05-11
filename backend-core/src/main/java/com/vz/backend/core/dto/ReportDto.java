package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReportDto {
	private ReportDocumentDto docIn;
	private ReportDocumentDto docOut;
}
