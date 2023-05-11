package com.vz.backend.business.service;

import com.vz.backend.business.domain.AttachmentReport;
import com.vz.backend.business.repository.IAttachmentReportRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.service.FilesStorageService;
import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

@Service
public class AttachmentReportService {

    @Autowired
    IAttachmentReportRepository iAttachmentReportRepository;

    @Autowired
    FilesStorageService storageService;

    public List<AttachmentReport> addAttachment(long reportId, MultipartFile[] files) {
        List<AttachmentReport> aList = new ArrayList<>();
        if (ArrayUtils.isEmpty(files)) {
            return Collections.emptyList();
        }
        for (MultipartFile f : files) {
            AttachmentReport a = new AttachmentReport();
            a.setName(storageService.save(f));
            a.setType(f.getContentType());
            a.setSize(f.getSize());
            a.setReportId(reportId);
            iAttachmentReportRepository.save(a);
            aList.add(a);
        }
        return aList;
    }

    public List<AttachmentReport> getAttachmentByReportId(Long reportId) {
        List<AttachmentReport> aList = iAttachmentReportRepository.getListAttachmentReportByReportId(BussinessCommon.getClientId(), true, reportId);
        return aList;
    }

    public AttachmentReport removeAttachmentReport(Long id) {
        AttachmentReport a = new AttachmentReport();
        a = iAttachmentReportRepository.getAttachmentReportById(BussinessCommon.getClientId(), id);
        if (a != null) {
            a.setActive(false);
            iAttachmentReportRepository.save(a);
            try {
                storageService.deleteFile(a.getName());
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
        return a;
    }
}
