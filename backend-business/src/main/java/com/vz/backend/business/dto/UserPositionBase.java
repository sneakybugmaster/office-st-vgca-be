package com.vz.backend.business.dto;

import com.vz.backend.core.dto.UserDto;

import lombok.Getter;

@Getter
public class UserPositionBase extends UserDto {
	private static final long serialVersionUID = 1L;

	private String positionName;
	private Integer positionOrder;

	public UserPositionBase(Long id, String userName, String fullName, String positionName, Integer positionOrder) {
		super(id, userName, fullName);
		this.positionName = positionName;
		this.positionOrder = positionOrder;
	}

	public static int sort(UserPositionBase a, UserPositionBase b) {
		int i = toInt(a.getPositionOrder()) - toInt(b.getPositionOrder());
		if (i != 0) {
			return i;
		}
		return a.getFullName().compareToIgnoreCase(b.getFullName());
	}

	private static int toInt(Integer input) {
		if (input == null) {
			return 0;
		}
		return input;
	}
}
