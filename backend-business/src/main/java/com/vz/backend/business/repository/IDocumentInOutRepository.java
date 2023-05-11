package com.vz.backend.business.repository;

import java.util.List;
import java.util.Set;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.DocumentInOut;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentInOutRepository extends IRepository<DocumentInOut> {

	List<DocumentInOut> findByDocOutIdAndClientId(Long docOutId, Long clientId);

	@Query("SELECT DISTINCT d.docInId FROM DocumentInOut d WHERE d.docOutId = :docOutId AND d.clientId = :clientId")
	Set<Long> findDocInIdByDocOutIdAndClientId(Long docOutId, Long clientId);

	@Modifying
	void deleteByDocOutIdAndDocInIdIn(Long docOutId, Set<Long> addIds);

}
