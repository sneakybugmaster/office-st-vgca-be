package com.vz.backend.business.config;

public enum HsFolderTrackingEnum {
	CREATE("Tạo mới"),
	UPDATE("Chỉnh sửa"),
	TRANSFER_PB("Nộp lưu trữ phòng ban"),
	TRANSFER_CQ("Nộp lưu trữ cơ quan"),
	FINISH("Kết thúc hồ sơ"),
	DUYET_PB("Duyệt hồ sơ phòng ban"),
	DUYET_CQ("Duyệt hồ sơ cơ quan"),
	TRA_LAI_PB("Trả lại hồ sơ phòng ban"),
	TRA_LAI_CQ("Trả lại hồ sơ cơ quan"),
	DELETE("Xóa");
	
	private final String key;

	HsFolderTrackingEnum(String key) {
		this.key = key;
	}

	public String getValue() {
		return key;
	}

	public static String getEnum(String value) {
		for (HsFolderTrackingEnum v : values()) {
			if (v.getValue().equalsIgnoreCase(value)) {
				return v.getValue();
			}
		}
		return null;
	}
}
