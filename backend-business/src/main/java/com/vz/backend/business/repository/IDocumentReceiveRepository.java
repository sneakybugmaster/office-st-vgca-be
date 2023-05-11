package com.vz.backend.business.repository;

import java.util.List;

import com.vz.backend.core.config.HandleTypeEnum;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.vz.backend.business.domain.DocumentReceive;
import com.vz.backend.business.dto.document.DocumentReceiveBasicDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IDocumentReceiveRepository extends IRepository<DocumentReceive> {

	@Query("SELECT d FROM DocumentReceive d WHERE d.docId = :docId AND d.clientId = :clientId")
	List<DocumentReceive> findByClientIdAndDocId(@Param(value = "clientId") Long clientId,
			@Param(value = "docId") Long docId);

	@Modifying
	@Transactional
	@Query("DELETE FROM DocumentReceive WHERE docId = :docId AND clientId = :clientId")
	void deleteByDocId(@Param(value = "docId") Long docId, @Param(value = "clientId") Long clientId);

	@Query("SELECT d FROM DocumentReceive d WHERE d.docId in (:docIds) AND d.clientId = :clientId")
	List<DocumentReceive> findByClientIdAndDocId(Long clientId, Long[] docIds);

	@Query("SELECT d FROM DocumentReceive d WHERE d.receiveId = (:userId) AND d.clientId = :clientId AND d.type =:type ")
	List<DocumentReceive> findByClientIdAndUserId(Long clientId, Long userId, String type);

	@Query("SELECT d FROM DocumentReceive d WHERE d.receiveId = (:userId) AND docId = :docId AND d.clientId = :clientId AND d.type =:type ")
	DocumentReceive findByClientIdAndDocIdAndUserId(Long clientId, Long docId, Long userId, String type);

	@Override
	@Modifying
	@Query("DELETE FROM DocumentReceive WHERE id = :id")
	void deleteById(Long id);

	@Query("SELECT new com.vz.backend.business.domain.DocumentReceive(d.id, d.receiveId, d.type) FROM DocumentReceive d WHERE d.clientId = :clientId AND d.docId = :docId and (:type is null or d.type = :type) and d.active = true")
	List<DocumentReceive> findByClientIdAndDocIdAndType(Long clientId, Long docId, String type);


	@Query("select dr from DocumentReceive dr where dr.docId = :docId and (:type is null or dr.type = :type) and dr.clientId = :clientId and dr.active = :active")
	List<DocumentReceive> findByDocIdAndTypeAndClientIdAndActive(Long docId, String type, Long clientId, Boolean active);

	@Query("SELECT new com.vz.backend.business.dto.document.DocumentReceiveBasicDto(d.receiveId, d.type) FROM DocumentReceive d WHERE d.clientId = :clientId AND d.docId = :docId and (:type is null or d.type = :type) and d.active = true")
	List<DocumentReceiveBasicDto> findDocumentReceiveDtoByClientIdAndDocIdAndType(Long clientId, Long docId, String type);
	
	@Modifying
	@Query("UPDATE FROM DocumentReceive set active = false WHERE id in (:idList) and docId = :docId and clientId = :clientId and active = true")
	void updateByIdList(Long docId, List<Long> idList, Long clientId);

	@Query("select count(distinct p.docId) from DocumentReceive p where " + " p.clientId = :clientId "
			+ "and ((p.type ='USER' AND p.receiveId =:id ) OR (p.type ='ORG' AND :lead is TRUE AND p.receiveId = :org))")
	Long countDocByUser(Long clientId, Long id, Long org, boolean lead);
	
	@Query("SELECT new com.vz.backend.business.domain.DocumentReceive(d.id, d.receiveId, d.type) FROM DocumentReceive d WHERE d.clientId = :clientId AND d.docId = :docId and d.type != 'FORWARD' and d.active = true")
	List<DocumentReceive> findByClientIdAndDocIdAndTypeNotTransfer(Long clientId, Long docId);

	@Query("SELECT d FROM DocumentReceive d WHERE d.clientId = :clientId AND d.docId = :docId and d.type != 'FORWARD' and (:handleType is null or d.handleType = :handleType) and d.active = true")
	List<DocumentReceive> findDocumentReceiveByDocIdAndTypeNotTransfer(Long clientId, Long docId, HandleTypeEnum handleType);

	DocumentReceive findByClientIdAndDocIdAndReceiveIdAndTypeAndActiveTrue(Long clientId, Long docId, Long userId,
			String type);

	List<DocumentReceive> findByClientIdAndTypeAndDocId(Long clientId, String recieveTypeForward, Long docId);

	@Query("SELECT new com.vz.backend.business.domain.DocumentReceive(d.id, d.receiveId, d.type) FROM DocumentReceive d WHERE d.clientId = :clientId AND d.docId = :docId and d.type IN ('FORWARD', 'USER') and d.active = true AND d.receiveId = :receiveId")
	List<DocumentReceive> findByClientIdAndDocIdAndReceiveIdAndActiveTrue(Long clientId, Long docId, Long receiveId);

	List<DocumentReceive> findByClientIdAndDocIdAndReceiveIdAndTypeInAndActiveTrue(Long clientId, Long docId,
			Long userId, String[] types);
}
