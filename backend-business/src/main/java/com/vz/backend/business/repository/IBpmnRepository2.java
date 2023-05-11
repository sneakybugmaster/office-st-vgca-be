package com.vz.backend.business.repository;

import java.util.List;

import com.vz.backend.business.domain.Condition;
import com.vz.backend.business.domain.DocumentInProcess;
import com.vz.backend.business.domain.NodeModel2;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.vz.backend.business.domain.BpmnModel2;
import com.vz.backend.business.domain.BpmnModel2.TYPE_DOCUMENT;
import com.vz.backend.business.dto.BpmnSearchDto;
import com.vz.backend.business.dto.UserConditionDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface IBpmnRepository2 extends IRepository<BpmnModel2> {

	List<BpmnModel2> findByClientIdAndActiveAndTypeDocument(Long clientId, boolean active, TYPE_DOCUMENT typeDocument);

	@Modifying()
	@Query("UPDATE BpmnModel2 b SET b.active=false WHERE (:bpmnId is NULL OR b.id!=:bpmnId) AND b.clientId=:clientId AND b.typeDocument=:typeDocument AND b.orgId=:orgId")
	void updateOneActiveForBpmnId(Long clientId, TYPE_DOCUMENT typeDocument, Long orgId, Long bpmnId);

	@Query("SELECT new com.vz.backend.business.dto.UserConditionDto(u.id, cate.isDefault, cate.isBreadth, cate.isSiblings, org.id, cate.id, cate.name, cate.order, u.fullName, u.userName, c.orgId, c.positionId, c.userId, c.forceSameOrg, org.name, second) "
			+ "FROM User u "
			+" inner join Organization org ON u.org=org.id AND org.active is true AND org.clientId =:clientId "
			+" INNER JOIN Category cate ON cate.id = u.position and cate.active is true AND cate.clientId =:clientId"
			+" inner join Condition c ON c.clientId=:clientId AND u.clientId=:clientId AND c.nodeId=:nodeId AND u.active is true "
			+ "left join UserCategory uc on u.id = uc.userId and uc.active is true "
			+ "left join Category second on uc.categoryId = second.id and second.active is true "
			+ "left join UserOrganization uo on u.id = uc.userId and uc.active is true "
			+ "left join Organization orgSecond on uo.orgId = orgSecond.id and orgSecond.active is true "
			+ "WHERE  (:text IS NULL OR LOWER(u.fullName) LIKE %:text%) "
			+ " AND u.active IS TRUE AND org.active IS TRUE AND u.fullName is not null "
			// condition orgType is setted or config user for condition
			+ " AND (c.userId IS NOT NULL OR c.orgType IS NULL OR c.orgType = org.orgType) "
			+ " AND (:subHandle IS TRUE OR c.subHandle IS FALSE) AND (c.userId=u.id OR (c.userId is NULL AND ("
			+ "  (c.orgId=u.org AND c.positionId is null AND cate.isDefault is TRUE) OR ((u.position=c.positionId) OR (uc.categoryId = c.positionId))"
			+ ")))"
			+ "ORDER BY cate.order, LOWER(u.fullName)")
	List<UserConditionDto> getUserByNodeId(Long nodeId, boolean subHandle, String text, Long clientId);

	@Query("SELECT new com.vz.backend.business.dto.BpmnSearchDto(b.id, b.active, b.name, b.typeDocument, b.org.id, b.org.name) "
			+ "FROM BpmnModel2 b WHERE (:type IS NULL OR b.typeDocument=:type) "
			+ "AND b.clientId=:clientId AND (:name IS NULL OR LOWER(b.name) LIKE %:name%)")
	Page<BpmnSearchDto> search(String name, TYPE_DOCUMENT type, Long clientId, Pageable pageable);

	@Query("SELECT count(*)>0 FROM BpmnModel2 b WHERE b.clientId=:clientId AND LOWER(b.name)=:name AND (:id is NULL OR b.id<>:id)")
	boolean existName(String name, Long id, Long clientId);

	@Query("select b FROM BpmnModel2 b where b.clientId=:clientId and b.active =:active and b.orgId in (:orgs) and b.typeDocument =:typeDocument")
	List<BpmnModel2> getByClientIdAndListOrg(Long clientId, Boolean active, List<Long> orgs, TYPE_DOCUMENT typeDocument);

	@Query("select count(1) > 0 from DocumentInProcess d where d.node in ( select n.id from NodeModel2 n where n.bpmnId = :bpmnId ) and d.clientId=:clientId and d.active is true")
	boolean checkBpmnIsUsedDocumentIn(Long bpmnId, Long clientId);

	@Query("select count(1) > 0 from DocumentOutProcess d where d.nodeId in ( select n.id from NodeModel2 n where n.bpmnId = :bpmnId ) and d.clientId=:clientId and d.active is true")
	boolean checkBpmnIsUsedDocumentOut(Long bpmnId, Long clientId);
}
