package com.vz.backend.business.repository.hstl;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.hstl.HsFolderRecordForm;
import com.vz.backend.business.dto.hstl.ecm.FormRegisterDto;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordFormListDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IHsFolderRecordFormRepository extends IRepository<HsFolderRecordForm>{

	@Query("SELECT NEW com.vz.backend.business.dto.hstl.ecm.HsFolderRecordFormListDto(f, false) "
			+ "FROM HsFolderRecordForm f WHERE f.active=TRUE AND f.clientId=:clientId AND f.status IN (:status) "
			+ "ORDER BY f.createDate DESC")
	Page<HsFolderRecordFormListDto> list(List<Integer> status, Long clientId, Pageable pageable);

	@Query("SELECT NEW com.vz.backend.business.dto.hstl.ecm.FormRegisterDto(f.id, f.name, f.dateResponse) "
			+ " FROM HsFolderRecordForm f WHERE f.active=TRUE AND f.clientId=:clientId AND f.status = 5")
	List<FormRegisterDto> all(Long clientId);

	@Query("SELECT COUNT(1) FROM HsFolderRecordForm f WHERE f.active=TRUE AND f.clientId=:clientId AND f.name = :name")
	long countByName(String name, Long clientId);

	@Query("SELECT f "
			+ " FROM HsFolderRecordForm f WHERE f.active=TRUE AND f.id =:id")
	HsFolderRecordForm findByActiveAndId(Long id);
}
