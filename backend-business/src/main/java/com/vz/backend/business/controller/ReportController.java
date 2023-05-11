package com.vz.backend.business.controller;

import com.google.common.net.HttpHeaders;
import com.vz.backend.business.domain.AttachmentReport;
import com.vz.backend.business.domain.AttachmentVersion;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.Report;
import com.vz.backend.business.dto.SearchReportDto;
import com.vz.backend.business.service.AttachmentReportService;
import com.vz.backend.business.service.ReportService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentOutTrackingEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.controller.AuthenController;
import com.vz.backend.core.controller.CategoryController;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.core.util.StreamUtils;
import com.vz.backend.util.StringUtils;
import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import java.nio.file.Files;
import java.util.Date;
import java.util.Optional;

@RestController
@RequestMapping("/report")
public class ReportController {

    @Autowired
    ReportService reportService;

    @Autowired
    AttachmentReportService attachmentReportService;

    @Autowired
    FilesStorageService storageService;

    @PostMapping("/search/{page}")
    public ResponseEntity<?> searchReport(@RequestBody SearchReportDto searchReportDto, @PathVariable Integer page) {
        return new ResponseEntity<>(reportService.searchReport(searchReportDto, page), HttpStatus.OK);
    }

    @PostMapping("/add")
    public ResponseEntity<?> searchReport(@RequestBody Report report) {
        return new ResponseEntity<>(reportService.addReport(report), HttpStatus.OK);
    }

    @PostMapping("/add/attachment/{reportId}")
    public ResponseEntity<?> retakeByStep(@PathVariable(value = "reportId") Long reportId,
                                          @RequestParam(required = false) MultipartFile[] files) {
        return new ResponseEntity<>(attachmentReportService.addAttachment(reportId, files), HttpStatus.OK);
    }

    @PostMapping("/update/{id}")
    public ResponseEntity<?> searchReport(@PathVariable("id") long id, @RequestBody Report report) {
        return new ResponseEntity<>(reportService.updateReport(id, report), HttpStatus.OK);
    }

    @GetMapping("/getReport/{id}")
    public ResponseEntity<?> getReport(@PathVariable("id") long id) {
        return new ResponseEntity<>(reportService.getReport(id), HttpStatus.OK);
    }

    @GetMapping("/report-approve/{id}/{status}")
    public ResponseEntity<?> reportApprove(@PathVariable("id") long id, @PathVariable("status") int status) {
        return new ResponseEntity<>(reportService.approveReport(id, status), HttpStatus.OK);
    }

    @GetMapping("/delete/{id}")
    public ResponseEntity<?> deleteApprove(@PathVariable("id") long id) {
        return new ResponseEntity<>(reportService.deleteReport(id), HttpStatus.OK);
    }

    @GetMapping("/download/{fileName:.+}")
    public ResponseEntity<StreamingResponseBody> getFile(@PathVariable String fileName) {
        String tmpName = StringUtils.decodeFromUrl(fileName);
        StreamingResponseBody stream;
        String outputName;

        Resource file = storageService.load(tmpName);
        if (file == null) {
            throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
        }
        stream = outputStream -> {
            try {
                Files.copy(file.getFile().toPath(), outputStream);
            } finally {
                StreamUtils.closeOutputStream(outputStream);
            }
        };
        outputName = FilesStorageService.origin(tmpName);

        return ResponseEntity.ok()
                .header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + outputName + "\"").body(stream);
    }

    @PostMapping("/exportData")
    public ResponseEntity<StreamingResponseBody> exportDataHeadings(@RequestParam long id) {
        StreamingResponseBody stream = outputStream -> {
            try {
                reportService.getDataExportWord(outputStream, id);
            } finally {
                StreamUtils.closeOutputStream(outputStream);
            }
        };
        return new ResponseEntity<>(stream, HttpStatus.OK);
    }
    @PostMapping("/exportData-all")
    public ResponseEntity<StreamingResponseBody> exportDataHeadings(@RequestBody SearchReportDto searchReportDto) {
        StreamingResponseBody stream = outputStream -> {
            try {
                reportService.getDataExportWordAll(outputStream,searchReportDto);
            } finally {
                StreamUtils.closeOutputStream(outputStream);
            }
        };
        return new ResponseEntity<>(stream, HttpStatus.OK);
    }

    @GetMapping("/deleteFile/{id}")
    public ResponseEntity<?> deleteFile(@PathVariable("id") long id) {
        AttachmentReport success = attachmentReportService.removeAttachmentReport(id);
        if (success != null) {
            return ResponseEntity.status(HttpStatus.OK).build();
        }

        return ResponseEntity.status(HttpStatus.EXPECTATION_FAILED).build();
    }

    @PostMapping(value = "/findUser")
    public ResponseEntity<?> findUser(@RequestParam(value = "position", required = false) String position,
                                      @RequestParam(defaultValue = "1", required = false) int page,
                                      @RequestParam(defaultValue = "10", required = false) int size) {
        Long positionT = position != null && position.length() > 0 ? Long.parseLong(position) : null;
        return new ResponseEntity<>(reportService.findUser(positionT, page, size),
                HttpStatus.OK);
    }
}
