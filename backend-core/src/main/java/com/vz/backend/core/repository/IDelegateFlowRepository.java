package com.vz.backend.core.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;

import com.vz.backend.core.domain.DelegateFlow;
import com.vz.backend.core.dto.DelegateFlowDto;

public interface IDelegateFlowRepository extends IRepository<DelegateFlow> {

	@Query("SELECT new com.vz.backend.core.dto.DelegateFlowDto(df.id, df.fromPositionModel.name, df.toPositionModel.name) FROM DelegateFlow df WHERE df.active is true AND df.clientId = :clientId")
	Page<DelegateFlowDto> list(Long clientId, Pageable pageable);

	@Query("SELECT df FROM DelegateFlow df WHERE df.active is true AND df.clientId = :clientId")
	List<DelegateFlow> list(Long clientId);

	@Query("SELECT df.fromPositionId FROM DelegateFlow df WHERE df.active is true AND df.clientId = :clientId")
	List<Long> listFromPosition(Long clientId);
}
