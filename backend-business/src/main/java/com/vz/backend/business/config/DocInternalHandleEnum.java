package com.vz.backend.business.config;

import lombok.Getter;

@Getter
public enum DocInternalHandleEnum {
	MAIN("Xử lý chính"),
	KNOW("Nhận để biết"),
	SUPPORT("Phối hợp"),
	EXECUTE("Thực hiện"),
	VIEW("Xem");

	private final String name;
	
	DocInternalHandleEnum(String name) {
		this.name = name;
	}
}
