package com.vz.backend.business.config;

public enum HsFolderProcessEnum {
	PB_CHO_DUYET("Chờ duyệt"),
	PB_DA_DUYET("Đã duyệt"),
	PB_TRA_LAI("Hồ sơ trả lại"),
	CQ_CHO_DUYET("Chờ duyệt"),
	CQ_DA_DUYET("Đã duyệt"),
	CQ_TRA_LAI("Hồ sơ trả lại");
	
	private final String key;

	HsFolderProcessEnum(String key) {
		this.key = key;
	}

	public String getValue() {
		return key;
	}

	public static String getEnum(String value) {
		for (HsFolderProcessEnum v : values()) {
			if (v.getValue().equalsIgnoreCase(value)) {
				return v.getValue();
			}
		}
		return null;
	}
}
