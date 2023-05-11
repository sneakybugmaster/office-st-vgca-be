package com.vz.backend.business.repository;

import java.util.List;

import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentUser;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentUserRepository extends IRepository<DocumentUser> {

	DocumentUser findByDocTypeAndDocIdAndUserIdAndClientId(DocumentTypeEnum docType, Long docId, Long userId,
			Long clientId);

	List<DocumentUser> findByDocTypeAndDocIdInAndUserIdAndClientId(DocumentTypeEnum docType, List<Long> idList,
			Long userId, Long clientId);

}
