package com.vz.backend.business.repository.hstl;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.hstl.HsFolderRecord;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordListDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IHsFolderRecordRepository extends IRepository<HsFolderRecord> {
	
	@Query("SELECT NEW com.vz.backend.business.dto.hstl.ecm.HsFolderRecordListDto(f, false) "
			+ "FROM HsFolderRecord f WHERE f.active=TRUE AND f.clientId=:clientId AND f.status IN (:status) ORDER BY f.createDate DESC")
	Page<HsFolderRecordListDto> list(List<Integer> status, Long clientId, Pageable pageable);

	HsFolderRecord findByIdAndClientIdAndActiveTrue(Long id, Long clientId);

	List<HsFolderRecord> findByClientIdAndAndIdInAndActiveTrue(Long clientId, List<Long> ids);

	@Query("SELECT f "
			+ "FROM HsFolderRecord f WHERE f.active=TRUE  AND f.id IN (:ids)")
	List<HsFolderRecord> listfindByActiveAndId(List<Long> ids);

	@Query("SELECT f "
			+ "FROM HsFolderRecord f WHERE f.active=TRUE  AND f.id=:id")
	HsFolderRecord findByActiveAndId(Long id);
}
