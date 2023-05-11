package com.vz.backend.business.repository;

import com.vz.backend.business.config.report.ReportTypeEnum;
import com.vz.backend.business.config.report.ReportTypeReportEnum;
import com.vz.backend.business.domain.Report;
import com.vz.backend.business.dto.SearchReportDto;
import com.vz.backend.core.repository.IRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IReportRepository extends IRepository<Report> {
    @Query("select r from Report r where r.clientId = :clientId " +
            "and (:reportTypeReportEnum is null or r.reportType = :reportTypeReportEnum) " +
            "and (:reportTypeEnum is null or r.type = :reportTypeEnum) " +
            "and (:year is null or r.year = :year) " +
            "and (:status is null or r.status = :status) " +
            "and (coalesce(:orgIds, null) is null  or r.orgId in (:orgIds)) " +
            "and r.active is true order by r.createDate DESC")
    Page<Report> searchReport(ReportTypeReportEnum reportTypeReportEnum, ReportTypeEnum reportTypeEnum,
                              Integer year, int status, Long clientId, List<Long>  orgIds, Pageable castToPageable);
    @Query("select count(1) > 0 from Report r where r.clientId = :clientId and r.active is true and r.confirmNumber=:confirmNumber")
    Boolean checkConfirmNumberExits(Long clientId, int confirmNumber);

    @Query("select r from Report r where r.clientId = :clientId " +
            "and (:reportTypeReportEnum is null or r.reportType = :reportTypeReportEnum) " +
            "and (:reportTypeEnum is null or r.type = :reportTypeEnum) " +
            "and (:year is null or r.year = :year) " +
            "and (:status is null or r.status = :status) " +
            "and (:week is null or r.week = :week) " +
            "and (coalesce(:orgIds, null) is null  or r.orgId in (:orgIds))  and r.active is true order by r.createDate DESC")
    List<Report> searchReportNotPagi(ReportTypeReportEnum reportTypeReportEnum, ReportTypeEnum reportTypeEnum,
                                     Integer year, int status, Long clientId, List<Long> orgIds ,Integer week);

}
