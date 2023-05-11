package com.vz.backend.business.config;

public enum FolderPermissionEnum {
	READ("Chỉ đọc"),
	FULL("Full");

	private final String key;

	FolderPermissionEnum(String key) {
		this.key = key;
	}

	public String getValue() {
		return key;
	}

	public static String getEnum(String value) {
		for (FolderPermissionEnum v : values()) {
			if (v.getValue().equalsIgnoreCase(value)) {
				return v.getValue();
			}
		}
		return null;
	}
}
