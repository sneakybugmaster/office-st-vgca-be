package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;

@Builder
@Getter
@AllArgsConstructor
public class MailNotiDto {
	private String fullName;
//	private String link;
	private String position;
	private String docType;
	private String preview;
	private String email;
	
	public static MailNotiDtoBuilder builder() {
		return new MailNotiDtoBuilder();
	}
}
