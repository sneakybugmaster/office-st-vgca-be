package com.vz.backend.business.config.ecabinet;

import lombok.Getter;

@Getter
public enum AttendanceTypeEnum {
	//Điểm danh
	WAITING("Chờ xác nhận"), ATTEND("Tham gia"), ABSENT("Vắng mặt"), REFUSE("Từ chối tham gia"),
	
	//Kiểu lời mời
	INDIVIDUAL("Cá nhân"), UNIT("Đơn vị");
	private final String name;

	AttendanceTypeEnum(String name) {
		this.name = name;
	}
}
