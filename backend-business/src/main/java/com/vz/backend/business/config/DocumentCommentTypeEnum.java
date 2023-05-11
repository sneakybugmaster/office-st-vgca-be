package com.vz.backend.business.config;

public enum DocumentCommentTypeEnum {

	XIN_Y_KIEN("Xin ý kiến"),
	CHUYEN_XU_LY("Chuyển xử lý"),
	CHUYEN_DON_VI("Chuyển đơn vị"),
	HOAN_THANH_XU_LY("Hoàn thành xử lý"),
	TRA_LAI("Trả lại"),
	THEM_XU_LY("Thêm xử lý"),
	;
	
	private final String name;

	DocumentCommentTypeEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
