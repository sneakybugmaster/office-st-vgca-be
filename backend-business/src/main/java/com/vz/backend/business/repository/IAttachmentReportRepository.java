package com.vz.backend.business.repository;

import com.vz.backend.business.domain.AttachmentReport;
import com.vz.backend.core.repository.IRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import java.util.List;

@Repository
public interface IAttachmentReportRepository extends IRepository<AttachmentReport> {

    @Query("select a from AttachmentReport a where a.clientId=:clientId and a.active=:active and a.reportId=:reportId")
    List<AttachmentReport> getListAttachmentReportByReportId(Long clientId, Boolean active, Long reportId);

    @Query("select a from AttachmentReport a where a.clientId=:clientId and a.id=:id")
    AttachmentReport getAttachmentReportById(Long clientId, Long id);
}
