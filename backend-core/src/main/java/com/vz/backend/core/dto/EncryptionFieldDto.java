package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class EncryptionFieldDto {
	private String key;
	private String encrypt;
	private Long userId;
}
