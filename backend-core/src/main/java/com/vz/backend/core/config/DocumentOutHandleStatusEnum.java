package com.vz.backend.core.config;

public enum DocumentOutHandleStatusEnum {
	DU_THAO("Dự thảo"), 
	DA_TRINH_KY("Đã trình ký"), 
	CHO_Y_KIEN("Chờ cho ý kiến"), 
	DA_Y_KIEN("Đã cho ý kiến"),
	CHO_XU_LY("Chờ xử lý"), 
	DA_XU_LY("Đã xử lý"), 
	DA_XU_LY_UQ("Đã xử lý"), 
	DA_THU_HOI("Đã thu hồi"),
	BI_THU_HOI("Đã bị thu hồi"),
	DA_TRA_LAI("Đã trả lại"), 
	DA_TRA_LAI_UQ("Đã trả lại"), 
	BI_TRA_LAI("Bị trả lại"), 
	THU_HOI_BH("Thu hồi");

	private final String name;

	DocumentOutHandleStatusEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}