package com.vz.backend.core.config;

public enum ObjectDataTypeEnum {
	CONGVAN("1"), GIAOVIEC("2"), HOSO("3");

	private final String key;

	ObjectDataTypeEnum(String key) {
		this.key = key;
	}

	public String getValue() {
		return key;
	}

	public static String getEnum(String value) {
		for (ObjectDataTypeEnum v : values()) {
			if (v.getValue().equalsIgnoreCase(value)) {
				return v.getValue();
			}
		}
		return null;
	}
}
