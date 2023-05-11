package com.vz.backend.core.config;

public enum DocumentOutActionEnum {
    UPDATE("Cập nhật"), CREATE("Tạo mới");

    private final String name;

    DocumentOutActionEnum(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
