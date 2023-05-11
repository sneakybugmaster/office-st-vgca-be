package com.vz.backend.business.config;

public enum IconTypeEnum {
	FOLDER("Folder"),
	PERSONAL("Cá nhân"),
	ORG("Cơ quan"),
	SHARE("Chia sẻ"),
	DOC("Văn bản"),
	FILE("File");

	private final String key;

	IconTypeEnum(String key) {
		this.key = key;
	}

	public String getValue() {
		return key;
	}

	public static String getEnum(String value) {
		for (IconTypeEnum v : values()) {
			if (v.getValue().equalsIgnoreCase(value)) {
				return v.getValue();
			}
		}
		return null;
	}
}
