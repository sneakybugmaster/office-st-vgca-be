package com.vz.backend.core.config;

public enum DeadlineHandleTypeEnum {
	SUPPERVISOR("Quản trị"), DELEGATE("Ủy quyền"), THREADS("Luồng"), ASSIGNER("Người giao việc"), EXECUTOR("Người nhận việc");

	private final String name;

	DeadlineHandleTypeEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
