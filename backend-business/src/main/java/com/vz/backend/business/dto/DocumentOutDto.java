package com.vz.backend.business.dto;

import com.vz.backend.business.domain.DocumentOut;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class DocumentOutDto {
	private DocumentOut docOut;
	private String personHandle;
}
