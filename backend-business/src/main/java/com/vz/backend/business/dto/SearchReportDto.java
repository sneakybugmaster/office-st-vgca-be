package com.vz.backend.business.dto;

import com.vz.backend.business.config.report.ReportTypeReportEnum;
import lombok.Data;

@Data
public class SearchReportDto {
    private String reportType;
    private String organization;
    private Integer year;
    private String type;
    private int status;

}
