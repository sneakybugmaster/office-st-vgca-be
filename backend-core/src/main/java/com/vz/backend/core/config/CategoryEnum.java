package com.vz.backend.core.config;

public enum CategoryEnum {
	DOCUMENT(1), DOCUMENT_OUT(3), LOGIN(2);

	private final Long key;

	CategoryEnum(long key) {
		this.key = key;
	}

	public long getValue() {
		return key;
	}

	public static Long getEnum(Long value) {
		for (CategoryEnum v : values()) {
			if (v.key.equals(value)) {
				return v.getValue();
			}
		}
		return null;
	}
}
