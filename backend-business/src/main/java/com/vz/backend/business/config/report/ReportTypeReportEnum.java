package com.vz.backend.business.config.report;

import com.vz.backend.business.config.FolderPermissionEnum;

public enum ReportTypeReportEnum {
    BAO_CAO_CHINH_QUYEN("Báo cáo chính quyền"), BAO_CAO_DANG("Báo cáo đảng");

    private final String name;

    ReportTypeReportEnum(String name) {
        this.name = name;
    }

    public String getValue() {
        return name;
    }

    public static String getEnum(String value) {
        for (ReportTypeReportEnum v : values()) {
            if (v.getValue().equalsIgnoreCase(value)) {
                return v.getValue();
            }
        }
        return null;
    }
}
