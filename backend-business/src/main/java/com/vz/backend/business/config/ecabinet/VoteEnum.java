package com.vz.backend.business.config.ecabinet;

public enum VoteEnum {

	/** Mẫu biểu quyết */
	VOTE("Biểu quyết"), CHOOSE_ONE("Phương án (Chọn 1 phương án"), CHOOSE_MULTI("Phương án (Chọn nhiều phương án"),

	/** Phân loại biểu quyết */
	INSIDE("Biểu quyết trên hệ thống"), OUTSIDE("Biểu quyết ngoài hệ thống"),

	/** Trạng thái biểu quyết */
	VOTING("Chờ biểu quyết"), VOTED("Đã biểu quyết");

	private String name;

	VoteEnum(String name) {
		this.name = name;
	}
}
