package com.vz.backend.business.dto.report;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReportContent {
    private String orgName;
    private String time;
    private String contentReport1;
    private String contentReport2;
    private String contentReport3;
}
