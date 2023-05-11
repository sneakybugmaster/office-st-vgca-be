package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.KPIApplication;
import com.vz.backend.business.dto.kpi.KPIApplicationDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IKPIApplicationRepository extends IRepository<KPIApplication>{

	@Query("SELECT k FROM KPIApplication k WHERE k.clientId = :clientId AND k.active = TRUE "
			+ " AND (:#{#dto.name} IS NULL OR LOWER(k.name) LIKE %:#{#dto.name}%)"
			+ " AND (:#{#dto.year} IS NULL OR k.year =:#{#dto.year})"
			+ " AND (:#{#dto.kpiSet} IS NULL OR k.kpiSet = :#{#dto.kpiSet})"
			+ " AND (:#{#dto.frequency} IS NULL OR k.frequency = :#{#dto.frequency})"
			+ " AND (:#{#dto.month} IS NULL OR k.months LIKE %:#{#dto.month}%)"
			+ " ORDER BY k.createDate DESC"
			)
	Page<KPIApplication> list(KPIApplicationDto dto, Long clientId, Pageable page);
	
	
	@Query("SELECT new com.vz.backend.business.dto.kpi.KPIApplicationDto(a.id, a.name, a.year) FROM KPIApplication a "
			+ " WHERE a.clientId = :clientId AND a.active = TRUE AND (:year IS NULL OR a.year = :year)")
	List<KPIApplicationDto> list(Integer year, Long clientId);

}
