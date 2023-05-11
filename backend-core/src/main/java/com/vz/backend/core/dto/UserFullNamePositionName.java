package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class UserFullNamePositionName {
	private String fullname;
	private String positionName;
	private String email;
}
