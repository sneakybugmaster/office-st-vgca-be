package com.vz.backend.business.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.KPI;
import com.vz.backend.business.dto.kpi.KPIDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IKPIRepository extends IRepository<KPI> {

	@Query("SELECT k FROM KPI k WHERE k.clientId =:clientId AND k.active = TRUE "
			+ " AND (:#{#dto.name} IS NULL OR LOWER(k.name) LIKE %:#{#dto.name}% )"
			+ " AND (:#{#dto.code} IS NULL OR LOWER(k.code) LIKE %:#{#dto.code}% )"
			+ " AND (:#{#dto.typeObj} IS NULL OR k.typeObj = :#{#dto.typeObj} )"
			)
	Page<KPI> list(KPIDto dto, Long clientId, Pageable page);

}
