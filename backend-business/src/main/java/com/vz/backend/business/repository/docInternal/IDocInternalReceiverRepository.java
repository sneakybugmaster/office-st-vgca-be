package com.vz.backend.business.repository.docInternal;

import java.util.List;

import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.documentInternal.DocInternalReceiver;
import com.vz.backend.business.dto.document.DocInternalReceiverDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocInternalReceiverRepository extends IRepository<DocInternalReceiver> {

	List<DocInternalReceiver> findByDocIdAndClientId(Long docId, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.document.DocInternalReceiverDto(d.id, d.type, d.userId, u.fullName, d.orgId, o.name, d.handleStatus) "
			+ "FROM DocInternalReceiver d LEFT JOIN User u ON u.id = d.userId "
			+ "LEFT JOIN Organization o ON o.id = d.orgId "
			+ "WHERE d.docId = :docId AND d.active is true AND d.clientId = :clientId")
	List<DocInternalReceiverDto> getListReceiverByDocIdAndClientId(Long docId, Long clientId);

	List<DocInternalReceiver> findByDocIdAndClientIdAndActiveTrue(Long docId, Long clientId);
}
