package com.vz.backend.business.config;

import com.vz.backend.core.config.DocumentInHandleStatusEnum;

import java.util.Arrays;
import java.util.Map;
import java.util.stream.Collectors;

public enum ViewStatusEnum {
    NEW("Chưa xem"), DUE("Đến hạn"), OVERDUE("Quá hạn"), SEEN("Đã xem"), HANDLED("Đã xử lý");
    private final String description;

    ViewStatusEnum(String description) {
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public static Map<ViewStatusEnum, String> getDescriptions() {
        return Arrays.stream(ViewStatusEnum.values()).collect(Collectors.toMap(e -> e, ViewStatusEnum::getDescription));
    }

}
