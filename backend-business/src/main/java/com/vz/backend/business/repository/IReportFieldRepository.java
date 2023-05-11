package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.ReportField;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IReportFieldRepository extends IRepository<ReportField> {

	List<ReportField> findByClientIdAndTypeAndActiveTrueOrderByOrderNumberDesc(Long clientId, DocumentTypeEnum type);

}
