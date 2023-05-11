package com.vz.backend.business.config.ecabinet;

public enum RoleEnum {
	HOST("Chủ trì"), MEMBER("Thành phần họp"), GUEST("Khách mời");

	private String name;

	RoleEnum(String name) {
		this.name = name;
	}
}
