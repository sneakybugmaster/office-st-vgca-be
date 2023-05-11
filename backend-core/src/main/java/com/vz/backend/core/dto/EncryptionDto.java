package com.vz.backend.core.dto;

import java.util.List;

import com.vz.backend.core.domain.Encryption;

import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
public class EncryptionDto {
	@Getter
	private List<Encryption> data;
}
