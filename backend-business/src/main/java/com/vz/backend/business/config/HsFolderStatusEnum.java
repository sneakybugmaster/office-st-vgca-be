package com.vz.backend.business.config;

public enum HsFolderStatusEnum {
	HS_TAI_LIEU("Hồ sơ tài liệu"),
	HS_CONG_VIEC("Hồ sơ công việc"),
	PB_CHO_DUYET("Chờ duyệt"),
	PB_DA_DUYET("Đã duyệt"),
	HS_TRA_LAI("Hồ sơ trả lại"),
	CQ_CHO_DUYET("Chờ duyệt"),
	CQ_DA_DUYET("Đã duyệt"),
	PB_TRA_LAI("Hồ sơ trả lại");
	
	private final String key;

	HsFolderStatusEnum(String key) {
		this.key = key;
	}

	public String getValue() {
		return key;
	}

	public static String getEnum(String value) {
		for (HsFolderStatusEnum v : values()) {
			if (v.getValue().equalsIgnoreCase(value)) {
				return v.getValue();
			}
		}
		return null;
	}
	
	public static HsFolderStatusEnum getEnumValue(String value) {
		for (HsFolderStatusEnum v : values()) {
			if (v.name().equalsIgnoreCase(value)) {
				return v;
			}
		}
		return null;
	}
}
