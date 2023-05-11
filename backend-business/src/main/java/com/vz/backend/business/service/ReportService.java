package com.vz.backend.business.service;

import com.vz.backend.business.config.report.ReportTypeEnum;
import com.vz.backend.business.config.report.ReportTypeReportEnum;
import com.vz.backend.business.domain.Report;
import com.vz.backend.business.dto.SearchReportDto;
import com.vz.backend.business.dto.report.PermissionDto;
import com.vz.backend.business.dto.report.ReportContent;
import com.vz.backend.business.dto.report.ReportPrintDto;
import com.vz.backend.business.repository.IReportRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AuthorityEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IAuthorityUserRepository;
import com.vz.backend.core.repository.ICategoryRepository;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.repository.IUserRepository;
import com.vz.backend.core.service.BaseService;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.core.service.UserService;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;
import org.jsoup.Jsoup;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;
import org.springframework.util.ResourceUtils;
import org.wickedsource.docxstamper.DocxStamper;
import org.wickedsource.docxstamper.DocxStamperConfiguration;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.WeekFields;
import java.util.*;

@Service
public class ReportService extends BaseService<Report> {

    @Autowired
    IReportRepository iReportRepository;

    @Autowired
    UserService userService;

    @Autowired
    ICategoryRepository categoryRepository;

    @Autowired
    CategoryService categoryService;

    @Autowired
    IAuthorityUserRepository iAuthorityUserRepository;

    @Autowired
    AttachmentReportService attachmentReportService;

    @Autowired
    OrganizationService organizationService;

    @Autowired
    IUserRepository userRepository;

    @Override
    public IRepository<Report> getRepository() {
        return this.iReportRepository;
    }

    public ListObjectDto<Report> searchReport(SearchReportDto searchReportDto, int page) {
        if (searchReportDto.getOrganization() != null && "".equals(searchReportDto.getOrganization().trim())) {
            searchReportDto.setOrganization(null);
        }
        List<Long> orgIds = getOrgCuc();
        if (searchReportDto.getOrganization() == null) {
            orgIds = getOrgCuc();
        } else {
            try {
                long orgId = Long.parseLong(searchReportDto.getOrganization());
                Organization organization = organizationService.findByClientIdAndId(orgId);
                orgIds = organizationService.orgAndSub(organization.getId());
            } catch (NumberFormatException e) {
            }
        }
        boolean checkVPB = false;
        if (iAuthorityUserRepository.checkAuthorByPermission(AuthorityEnum.REPORT_VP_BAN, BussinessCommon.getClientId(), BussinessCommon.getUserId())) {
            checkVPB = true;
        }
        ReportTypeReportEnum reportTypeReportEnum = checkReportType(searchReportDto);
        ReportTypeEnum reportTypeEnum = checkType(searchReportDto);
        Page<Report> reports = iReportRepository.searchReport(reportTypeReportEnum, reportTypeEnum, searchReportDto.getYear(),
                searchReportDto.getStatus(), BussinessCommon.getClientId(), checkVPB == true ? null : orgIds, BussinessCommon.castToPageable(page));
        for (Report report : reports.getContent()) {
            report.setSigner(userService.findByClientIdAndId(BussinessCommon.getClientId(), report.getSignerId()));
            report.setPositionTitle(categoryService.findByClientIdAndId(BussinessCommon.getClientId(), report.getPositionTitleId()));
            report.setAttachments(attachmentReportService.getAttachmentByReportId(report.getId()));
            report.setPermissionDto(setButtonPermission(report));
        }
        return BussinessCommon.paging(reports);
    }

    public List<Long> getOrgCuc() {
        Organization organization = organizationService.getOrgCucByOrg(BussinessCommon.getUser().getOrgModel());
        List<Long> orgIds = organizationService.orgAndSub(organization.getId());
        return orgIds;
    }

    public Report addReport(Report report) {
        if (iReportRepository.checkConfirmNumberExits(BussinessCommon.getClientId(), report.getConfirmNumber())) {
            throw new RestExceptionHandler(Message.CONFIRM_NUMBER);
        }
        if (StringUtils.isNullOrEmpty(report.getTitle(), true)) {
            throw new RestExceptionHandler(Message.REPORT_TITLE_NOT_FOUND);
        }
        if (!checkSignerExits(report)) {
            throw new RestExceptionHandler(Message.REPORT_SIGNER_NOT_FOUND);
        }
        if (!checkPositionTitle(report)) {
            throw new RestExceptionHandler(Message.REPORT_POSITION_TITLE_NOT_FOUND);
        }
        report.setStatus(1);
        report.setOrgId(BussinessCommon.getUser().getOrg());
        iReportRepository.save(report);
        return report;
    }

    public Report updateReport(long id, Report reportNew) {
        Report reportOld = getReport(id);
        if (reportNew.getConfirmNumber() != reportOld.getConfirmNumber() && iReportRepository.checkConfirmNumberExits(BussinessCommon.getClientId(), reportNew.getConfirmNumber())) {
            throw new RestExceptionHandler(Message.CONFIRM_NUMBER);
        }
        if (reportOld.getStatus() == 2) {
            throw new RestExceptionHandler(Message.REPORT_DUYET_NOT_UPDATE);
        }
        if (StringUtils.isNullOrEmpty(reportNew.getTitle(), true)) {
            throw new RestExceptionHandler(Message.REPORT_TITLE_NOT_FOUND);
        }
        if (!checkSignerExits(reportNew)) {
            throw new RestExceptionHandler(Message.REPORT_SIGNER_NOT_FOUND);
        }
        if (!checkPositionTitle(reportNew)) {
            throw new RestExceptionHandler(Message.REPORT_POSITION_TITLE_NOT_FOUND);
        }
        setReport(reportOld, reportNew);
        return iReportRepository.save(reportOld);
    }

    public Report getReport(long id) {
        Report report = iReportRepository.findByClientIdAndId(BussinessCommon.getClientId(), id);
        if (report == null) {
            throw new RestExceptionHandler(Message.REPORT_NOT_FOUND);
        } else {
            report.setSigner(userService.findByClientIdAndId(BussinessCommon.getClientId(), report.getSignerId()));
            report.setPositionTitle(categoryService.findByClientIdAndId(BussinessCommon.getClientId(), report.getPositionTitleId()));
            report.setAttachments(attachmentReportService.getAttachmentByReportId(id));
            report.setPermissionDto(setButtonPermission(report));
        }
        return report;
    }

    public ReportTypeReportEnum checkReportType(SearchReportDto report) {
        try {
            if (!StringUtils.isNullOrEmpty(report.getReportType())) {
                return ReportTypeReportEnum.valueOf(report.getReportType());
            } else {
                return null;
            }
        } catch (Exception e) {
            throw new RestExceptionHandler(Message.REPORT_TYPE_REPORT);
        }
    }

    public ReportTypeEnum checkType(SearchReportDto report) {
        try {
            if (!StringUtils.isNullOrEmpty(report.getType())) {
                return ReportTypeEnum.valueOf(report.getType());
            } else {
                return null;
            }
        } catch (Exception e) {
            throw new RestExceptionHandler(Message.REPORT_TYPE);
        }
    }

    public boolean checkSignerExits(Report report) {
        if (report == null || report.getSignerId() == null) return false;
        else {
            User user = userService.findByClientIdAndId(BussinessCommon.getClientId(), report.getSignerId());
            if (user == null) return false;
            else return true;
        }
    }

    public boolean checkPositionTitle(Report report) {
        if (report == null || report.getPositionTitleId() == null) return false;
        else
            return categoryRepository.checkPositionExits(Constant.CAT_POSITION, BussinessCommon.getClientId(), report.getPositionTitleId());
    }

    public void setReport(Report reportOld, Report reportNew) {
        reportOld.setTitle(reportNew.getTitle());
        reportOld.setPlaceReceive(reportNew.getPlaceReceive());
        reportOld.setReportType(reportNew.getReportType());
        reportOld.setType(reportNew.getType());
        reportOld.setConfirmDate(reportNew.getConfirmDate());
        reportOld.setSignerId(reportNew.getSignerId());
        reportOld.setWeek(reportNew.getWeek());
        reportOld.setYear(reportNew.getYear());
        reportOld.setWorkDone(reportNew.getWorkDone());
        reportOld.setOrganization(reportNew.getOrganization());
        reportOld.setConfirmNumber(reportNew.getConfirmNumber());
        reportOld.setStartDate(reportNew.getStartDate());
        reportOld.setEndDate(reportNew.getEndDate());
        reportOld.setRequestAttach(reportNew.getRequestAttach());
        reportOld.setExpected(reportNew.getExpected());
        reportOld.setPositionTitleId(reportNew.getPositionTitleId());
    }

    public void checkPermissonReport(Report report) {
        if (iAuthorityUserRepository.checkAuthorByPermission(AuthorityEnum.REPORT_VP_BAN, BussinessCommon.getClientId(), BussinessCommon.getUserId())) {

        } else if (report.getReportType() == ReportTypeReportEnum.BAO_CAO_CHINH_QUYEN) {
            if (!iAuthorityUserRepository.checkAuthorByPermission(AuthorityEnum.REPORT_CHINH_QUYEN, BussinessCommon.getClientId(), BussinessCommon.getUserId())) {
                throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);
            }
        } else if (report.getReportType() == ReportTypeReportEnum.BAO_CAO_DANG) {
            if (!iAuthorityUserRepository.checkAuthorByPermission(AuthorityEnum.REPORT_DANG, BussinessCommon.getClientId(), BussinessCommon.getUserId())) {
                throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);
            }
        }
    }

    public Report approveReport(Long id, int status) {
        Report report = getReport(id);
        checkPermissonReport(report);
        if (status != 1 && status != 2) {
            throw new RestExceptionHandler(Message.ACTION_STATUS);
        }
        report.setStatus(status);
        report.setConfirmDate(new Date());
        return iReportRepository.save(report);
    }

    public Report deleteReport(Long id) {
        Report report = getReport(id);
        if (!editOrDelPermission(report)) {
            throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);
        }
        report.setActive(false);
        return iReportRepository.save(report);
    }

    public boolean editOrDelPermission(Report report) {
        try {
            checkPermissonReport(report);
            return true;
        } catch (RestExceptionHandler e) {
            if (report.getStatus() == 1 && (report.getCreateBy().compareTo(BussinessCommon.getUserId()) == 0)) {
                return true;
            } else {
                return false;
            }
        }
    }

    public boolean approvePermission(Report report) {
        try {
            checkPermissonReport(report);
            return true;
        } catch (RestExceptionHandler e) {
            return false;
        }
    }

    public PermissionDto setButtonPermission(Report report) {
        PermissionDto permissionDto = new PermissionDto();
        permissionDto.setEditButton(editOrDelPermission(report));
        permissionDto.setDeleteButton(editOrDelPermission(report));
        permissionDto.setApproveButton(approvePermission(report));
        return permissionDto;
    }

    public void getDataExportWord(OutputStream outputStream, long id) {
        Report report = getReport(id);
        ReportPrintDto dto = new ReportPrintDto();
        Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
        String date = String.valueOf(c.get(Calendar.DATE));
        String month = String.valueOf(c.get(Calendar.MONTH) + 1);
        String year = String.valueOf(c.get(Calendar.YEAR));
        dto.setDate(date);
        dto.setMonth(month);
        dto.setYear(year);
        Organization orgModel = organizationService.getOrgCucByOrg(BussinessCommon.getUser().getOrgModel());
        dto.setOrgNameUpper(orgModel != null ? orgModel.getName().toUpperCase() : "");
        dto.setOrgNameLower(orgModel != null ? orgModel.getName() : "");
        if (report != null) {
            dto.setType(report.getType().getValue().toUpperCase());
            dto.setWorkDone(html2text(report.getWorkDone()));
            dto.setExpected(html2text(report.getExpected()));
            dto.setRequestAttach(html2text(report.getRequestAttach()));
            User user = userService.findByClientIdAndId(BussinessCommon.getClientId(), report.getSignerId());
            Category category = categoryService.findByClientIdAndId(BussinessCommon.getClientId(), report.getPositionTitleId());
            if (user != null) {
                dto.setUserName(user.getFullName());

            }
            if (report.getType() == ReportTypeEnum.YEAR || report.getType() == ReportTypeEnum.FIRST_6_MONTH || report.getType() == ReportTypeEnum.LAST_6_MONTH) {
                dto.setTypeLower("Năm ");
            } else {
                dto.setTypeLower(report.getType().getValue() + " " + report.getWeek() + "/");
            }

            dto.setWeek(report.getYear() + "");
            SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy");
            dto.setFromDate(dateFormat.format(report.getStartDate()));
            dto.setToDate(dateFormat.format(report.getEndDate()));
            if (category != null) {
                dto.setPosition(category.getName().toUpperCase());
            }
        }
        try {
            DocxStamperConfiguration stamperConfig = new DocxStamperConfiguration();
            stamperConfig.setLineBreakPlaceholder("##");
            DocxStamper<ReportPrintDto> stamper = stamperConfig.build();
            File fs = ResourceUtils.getFile("classpath:templates/Báo cáo.docx");
            try (FileInputStream fis = new FileInputStream(fs)) {
                stamper.stamp(fis, dto, outputStream);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void getDataExportWordAll(OutputStream outputStream, SearchReportDto searchReportDto) {
        ReportPrintDto dto = new ReportPrintDto();
        SimpleDateFormat dateFormat = new SimpleDateFormat("dd/MM/yyyy HH:mm");
        Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
        String date = String.valueOf(c.get(Calendar.DATE));
        String month = String.valueOf(c.get(Calendar.MONTH) + 1);
        String year = String.valueOf(c.get(Calendar.YEAR));
        dto.setDate(date);
        dto.setMonth(month);
        dto.setYear(year);
        ReportTypeEnum readyStatus = ReportTypeEnum.valueOf(searchReportDto.getType());
        dto.setType(readyStatus.getValue().toUpperCase());
        Organization orgModel = organizationService.getOrgCucByOrg(BussinessCommon.getUser().getOrgModel());
        dto.setOrgNameUpper(orgModel != null ? orgModel.getName().toUpperCase() : "");
        dto.setOrgNameLower(orgModel != null ? orgModel.getName() : "");


        dto.setWeek(year + "");
        List<ReportContent> reportContentList = new ArrayList<>();
        List<Report> listReport = listDataExport(searchReportDto, dto);
        for (int i = 0; i < listReport.size(); i++) {
            ReportContent reportContent = new ReportContent();
            Organization organizationUser = organizationService.getOrgByUserCreate(listReport.get(i).getCreateBy());
            Organization orgCuc = organizationService.getOrgCucByOrg(organizationUser);
            reportContent.setOrgName(orgCuc != null ? orgCuc.getName() : "");
            reportContent.setTime(dateFormat.format(listReport.get(i).getUpdateDate()));
            reportContent.setContentReport1(html2text(listReport.get(i).getWorkDone()));
            reportContent.setContentReport2(html2text(listReport.get(i).getExpected()));
            reportContent.setContentReport3(html2text(listReport.get(i).getRequestAttach()));
            reportContentList.add(reportContent);
        }
        dto.setWorkDoneAll(reportContentList);
        try {
            DocxStamperConfiguration stamperConfig = new DocxStamperConfiguration();
            stamperConfig.setLineBreakPlaceholder("##");
            DocxStamper<ReportPrintDto> stamper = stamperConfig.build();
            File fs = ResourceUtils.getFile("classpath:templates/Tổng hợp báo cáo.docx");
            try (FileInputStream fis = new FileInputStream(fs)) {
                stamper.stamp(fis, dto, outputStream);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public LocalDate getFirstDayOfWeek(int weekNumber, Locale locale, int year) {
        return LocalDate
                .of(year, 2, 1)
                .with(WeekFields.of(locale).getFirstDayOfWeek())
                .with(WeekFields.of(locale).weekOfWeekBasedYear(), weekNumber);
    }

    public String html2text(String html) {
        return Jsoup.parse(html).wholeText();
    }

    public ListObjectDto<User> findUser(Long position, int page, int total) {
        Organization orgCode = organizationService.getOrgCucByOrg(BussinessCommon.getUser().getOrgModel());
        List<Long> orgIds = orgCode.getId() == null ? null : organizationService.orgAndSub(orgCode.getId());
        Page<User> reports = userRepository.findUserByPositionAndOrg(orgIds, position, BussinessCommon.getClientId(), PageRequest.of(page - 1, total));
        return BussinessCommon.paging(reports);
    }

    public List<Report> listDataExport(SearchReportDto searchReportDto, ReportPrintDto dto) {
        SimpleDateFormat dateFormat= new SimpleDateFormat("dd/MM/yyyy");
        if (searchReportDto.getOrganization() != null && "".equals(searchReportDto.getOrganization().trim())) {
            searchReportDto.setOrganization(null);
        }
        List<Long> orgIds = getOrgCuc();
        if (searchReportDto.getOrganization() == null) {
            orgIds = getOrgCuc();
        } else {
            try {
                long orgId = Long.parseLong(searchReportDto.getOrganization());
                Organization organization = organizationService.findByClientIdAndId(orgId);
                orgIds = organizationService.orgAndSub(organization.getId());
            } catch (NumberFormatException e) {
            }
        }
        boolean checkVPB = false;
        if (iAuthorityUserRepository.checkAuthorByPermission(AuthorityEnum.REPORT_VP_BAN, BussinessCommon.getClientId(), BussinessCommon.getUserId())) {
            checkVPB = true;
        }
        ReportTypeReportEnum reportTypeReportEnum = checkReportType(searchReportDto);
        ReportTypeEnum reportTypeEnum = checkType(searchReportDto);
        Calendar c = Calendar.getInstance(DateTimeUtils.timeZone());
        Integer weekSeatch = null;
        if(searchReportDto.getType().equals("WEEK") ){
            weekSeatch = c.get(Calendar.WEEK_OF_YEAR);
        }else if(searchReportDto.getType().equals("MONTH") ){
            weekSeatch = c.get(Calendar.MONTH)+1;
            dto.setTypeLower("Năm ");
            dto.setFromDate(dateFormat.format(DateTimeUtils.getFirstDayOfMonth(new Date())));
            dto.setToDate(dateFormat.format(DateTimeUtils.getLastDayOfMonth(new Date())));
        }
        List<Report> reports = iReportRepository.searchReportNotPagi(reportTypeReportEnum, reportTypeEnum, searchReportDto.getYear(),
                searchReportDto.getStatus(), BussinessCommon.getClientId(), checkVPB == true ? null : orgIds, weekSeatch);
        dto.setPosition("");
        dto.setUserName("");
        for (int i = 0; i <reports.size(); i++) {
            if(searchReportDto.getType().equals("WEEK")){
                dto.setTypeLower("Tuần " + (c.get(Calendar.WEEK_OF_YEAR) - 1) + "/");
                LocalDate startDatOfWeek = getFirstDayOfWeek(c.get(Calendar.WEEK_OF_YEAR) - 1, Locale.UK, c.get(Calendar.YEAR));
                LocalDate lastDayOfWeek = startDatOfWeek.plusDays(6);
                dto.setFromDate(startDatOfWeek.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")));
                dto.setToDate(lastDayOfWeek.format(DateTimeFormatter.ofPattern("dd/MM/yyyy")));
                break;
            }else if(searchReportDto.getType().equals("MONTH")){
                dto.setTypeLower("Tháng " + (reports.get(0).getStartDate().getMonth()+1) + "/");
                dto.setFromDate(dateFormat.format(reports.get(0).getStartDate()));
                dto.setToDate(dateFormat.format(reports.get(0).getEndDate()));
                break;
            }else{
                dto.setTypeLower("Năm ");
                dto.setFromDate(dateFormat.format(reports.get(0).getStartDate()));
                dto.setToDate(dateFormat.format(reports.get(0).getEndDate()));
            }

        }
        return reports;
    }
}
