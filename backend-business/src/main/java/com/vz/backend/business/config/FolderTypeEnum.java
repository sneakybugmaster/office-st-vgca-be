package com.vz.backend.business.config;

public enum FolderTypeEnum {
	CANHAN("Cá nhân"),
	COQUAN("Cơ quan"),
	LUU_TRU_VINH_VIEN("Lưu trữ vĩnh viễn"),
	LUU_TRU_CO_QUAN("Lưu trữ cơ quan"),
	LUU_TRU_CA_NHAN("Lưu trữ cá nhân"),
	;

	private final String key;

	FolderTypeEnum(String key) {
		this.key = key;
	}

	public String getValue() {
		return key;
	}

	public static String getEnum(String value) {
		for (FolderTypeEnum v : values()) {
			if (v.getValue().equalsIgnoreCase(value)) {
				return v.getValue();
			}
		}
		return null;
	}
}
