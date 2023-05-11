package com.vz.backend.core.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.core.domain.Formula;
import com.vz.backend.core.dto.FormulaDto;

@Repository
public interface IFormulaRepository extends IRepository<Formula>{

	@Query("SELECT f FROM Formula f WHERE f.clientId =:clientId AND f.active = TRUE "
			+ " AND (:#{#dto.name} IS NULL OR LOWER(f.name) LIKE %:#{#dto.name}%) "
			+ " AND (:#{#dto.type} IS NULL OR LOWER(f.type) LIKE %:#{#dto.type}%) ")
	Page<Formula> list(FormulaDto dto, Long clientId, Pageable pageable);

	@Query("SELECT new com.vz.backend.core.dto.FormulaDto(f.id, f.name) FROM Formula f WHERE f.clientId =:clientId AND f.active = TRUE")
	List<FormulaDto> list(Long clientId);
}
