package com.vz.backend.business.repository;

import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.AttachmentVersion;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IAttachmentVersionRepository extends IRepository<AttachmentVersion> {

	@Transactional
	@Modifying
	@Query("DELETE FROM AttachmentVersion at WHERE at.docId = :docId")
	void deleteByDocId(@Param("docId") Long docId);

	@Query("SELECT d FROM AttachmentVersion d WHERE d.active = true AND d.docId = :docId AND d.clientId = :clientId ORDER BY d.version DESC")
	List<AttachmentVersion> findAllByDocId(Long docId, Long clientId);

	@Query("SELECT count(*)>0 FROM AttachmentVersion d WHERE d.active = true AND d.docId = :docId AND d.clientId = :clientId")
	boolean existAttachmentByDocIdAndClientId(long docId, Long clientId);

	@Query("SELECT count(*)>0 FROM AttachmentVersion d WHERE d.active = true AND d.name = :fileName AND d.docId = :docId AND d.clientId = :clientId")
	boolean existAttachmentByFileNameAndDocIdAndClientId(String fileName, long docId, Long clientId);

	Optional<AttachmentVersion> findByName(String fileName);

	@Query("SELECT max(d.version) FROM AttachmentVersion d WHERE d.active = true AND d.originName = :fileName AND d.docId = :docId AND d.clientId = :clientId")
	Integer getMaxVersionByNameAndDocId(String fileName, long docId, Long clientId);

	@Query("SELECT max(d.version) FROM AttachmentVersion d WHERE d.active = true AND d.docId = :docId AND d.clientId = :clientId")
	Integer getMaxVersionByDocId( long docId, Long clientId);
}
