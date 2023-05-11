package com.vz.backend.business.domain;

import com.vz.backend.business.config.report.ReportTypeEnum;
import com.vz.backend.business.config.report.ReportTypeReportEnum;
import com.vz.backend.business.dto.report.PermissionDto;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;
import java.util.Date;
import java.util.List;

@Entity
@Table(name = "REPORT", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Report extends BaseModel {
    private static final long serialVersionUID = 1L;

    @Column(name = "title")
    private String title;

    @Column(name = "report_type")
    @Enumerated(EnumType.STRING)
    private ReportTypeReportEnum reportType;

    @Column(name = "type")
    @Enumerated(EnumType.STRING)
    private ReportTypeEnum type;

    @Column(name = "year")
    private int year;

    @Column(name = "status")
    private int status;

    @Column(name = "week")
    private int week;

    @Column(name = "start_date")
    private Date startDate;

    @Column(name = "end_Date")
    private Date endDate;

    @Column(name = "work_done", columnDefinition = "TEXT") // những việc đã làm
    private String workDone;

    @Column(name = "expected", columnDefinition = "TEXT") // dự kiến kế hoạch sắp tới
    private String expected;

    @Column(name = "request_attach", columnDefinition = "TEXT") // Kiến nghị
    private String requestAttach;

    @Column(name = "confirm_number")
    private int confirmNumber;

    @Column(name = "organization")
    private String organization;

    @Column(name = "position_title")
    private Long positionTitleId;

    @Column(name = "place_receive")
    private Long placeReceive;

    @Column(name = "signer")
    private Long signerId;

    @Column(name = "confirm_date")
    private Date confirmDate;

    @Column(name = "org_id")
    private Long orgId;

    @Transient
    List<AttachmentReport> attachments;

    @Transient
    User Signer;

    @Transient
    Category positionTitle;

    @Transient
    PermissionDto permissionDto;
}
