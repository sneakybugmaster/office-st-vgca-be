package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class ConfigSignDto {
	private Long orgId;
	private String place;
	private String tl;
	private String signer;
	private String signature;
	private String position;
}
