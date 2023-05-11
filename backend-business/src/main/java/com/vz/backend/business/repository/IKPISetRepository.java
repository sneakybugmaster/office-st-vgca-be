package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.KPISet;
import com.vz.backend.business.dto.kpi.KPISetDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IKPISetRepository extends IRepository<KPISet> {

	@Query("SELECT s FROM KPISet s WHERE s.clientId=:clientId AND s.active = TRUE "
			+ " AND (:#{#dto.name} IS NULL OR LOWER(s.name) LIKE %:#{#dto.name}% )" 
			+ " AND (:#{#dto.formula} IS NULL OR s.formula =:#{#dto.formula} )"
			)
	Page<KPISet> list(KPISetDto dto, Long clientId, Pageable page);

	@Query("SELECT new com.vz.backend.business.dto.kpi.KPISetDto(s.id, s.name) FROM KPISet s "
			+ " WHERE s.clientId=:clientId AND s.active = TRUE "
			)
	List<KPISetDto> list(Long clientId);
}
