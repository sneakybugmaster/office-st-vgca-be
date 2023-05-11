package com.vz.backend.core.config;

public enum CalendarActionEnum {
	ADD("Tạo mới"), UPDATE("Cập nhật"), APPROVE("Phê duyệt"), REGISTER_TOP_LEVEL("Đăng kí lên Ban");
	
	private final String name;

	CalendarActionEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
