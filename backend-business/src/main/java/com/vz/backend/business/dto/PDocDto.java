package com.vz.backend.business.dto;

import com.vz.backend.core.config.HandleTypeEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class PDocDto {
	private String toUser;
	private String frUser;
	private HandleTypeEnum handleType;
	private int step;
	private Long frUsers;
	private Long node;
	private Long toUsers;

	public PDocDto(String toUser, String frUser, HandleTypeEnum handleType, int step) {
		this.toUser = toUser;
		this.frUser = frUser;
		this.handleType = handleType;
		this.step = step;
	}

	public PDocDto(Long frUsers, HandleTypeEnum handleType, int step) {
		this.frUsers = frUsers;
		this.handleType = handleType;
		this.step = step;
	}
	
	public PDocDto(Long frUsers, int step) {
		this.frUsers = frUsers;
		this.step = step;
	}
}
