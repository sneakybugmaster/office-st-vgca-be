package com.vz.backend.business.config.report;

public enum ReportTypeEnum {
    WEEK("Tuần"),
    MONTH("Tháng"),
    QUARTER("Quý"),
    FIRST_6_MONTH("6 tháng đầu năm"),
    YEAR("Báo cáo năm"),
    LAST_6_MONTH("6 tháng cuối năm");

    private final String name;

    ReportTypeEnum(String name) {
        this.name = name;
    }

    public String getValue() {
        return name;
    }
}
