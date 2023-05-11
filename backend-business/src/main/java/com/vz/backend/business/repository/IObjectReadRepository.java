package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.ObjectRead;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IObjectReadRepository extends IRepository<ObjectRead> {

	ObjectRead findFirstByClientIdAndUserIdAndObjIdAndTypeAndActiveTrue(Long clientId, Long userId, Long objId,
			DocumentTypeEnum type);

	List<ObjectRead> findByClientIdAndUserIdAndObjIdInAndTypeAndActiveTrue(Long clientId, Long userId,
			List<Long> objIds, DocumentTypeEnum type);
	
	long countByClientIdAndUserIdAndObjIdInAndTypeAndReadTrueAndActiveTrue(Long clientId, Long userId,
			List<Long> objIds, DocumentTypeEnum type);
}
