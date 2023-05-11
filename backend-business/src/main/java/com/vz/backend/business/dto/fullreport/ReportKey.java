package com.vz.backend.business.dto.fullreport;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ReportKey {
	private String key;
	private int year;
	private int part;
}
