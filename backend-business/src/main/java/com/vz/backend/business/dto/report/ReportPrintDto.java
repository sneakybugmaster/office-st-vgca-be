package com.vz.backend.business.dto.report;

import lombok.Data;

import java.util.List;

@Data
public class ReportPrintDto {
    private String week;
    private String date;
    private String month;
    private String year;
    private String fromDate;
    private String toDate;
    private String orgNameLower;
    private String orgNameUpper;
    private String workDone;
    private String expected;
    private String requestAttach;
    private String userName;
    private String position;
    private String type;
    private String typeLower;

    private List<ReportContent> workDoneAll;
}
