package com.vz.backend.core.config;

public enum DataTypeEnum {
	TEXT("text"), DATE("date"), NUMBER("number"), DATETIME("datetime-local"), CHECKBOX("checkbox"), RADIO("radio"),
	AUTOCOMPLETE("autocomplete"), TEXTAREA("textarea");

	private final String key;

	DataTypeEnum(String key) {
		this.key = key;
	}

	public String getValue() {
		return key;
	}

	public static String getEnum(String value) {
		for (DataTypeEnum v : values()) {
			if (v.getValue().equalsIgnoreCase(value)) {
				return v.getValue();
			}
		}
		return null;
	}
}
