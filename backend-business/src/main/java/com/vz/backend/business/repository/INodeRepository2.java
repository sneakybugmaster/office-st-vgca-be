package com.vz.backend.business.repository;

import java.util.List;
import java.util.Set;

import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.lang.NonNull;
import org.springframework.lang.Nullable;
import org.springframework.stereotype.Repository;

import com.vz.backend.business.domain.BpmnModel2.TYPE_DOCUMENT;
import com.vz.backend.business.domain.Condition;
import com.vz.backend.business.domain.NodeModel2;
import com.vz.backend.business.dto.BpmnActiveWrapper;
import com.vz.backend.business.dto.NodeDto;
import com.vz.backend.core.repository.IRepository;

@Repository
public interface INodeRepository2 extends IRepository<NodeModel2> {

    @Query("SELECT distinct new com.vz.backend.business.dto.NodeDto(flow.next.id, flow.next.name, flow.next.type, flow.next.allowMultiple, flow.name, bp.orgId, bp.org.name) " +
            "FROM NodeModel2 flow INNER JOIN BpmnModel2 bp on bp.id=flow.bpmnId WHERE flow.prevId=:prevId "
    )
    List<NodeDto> findAutoNextById(@Param("prevId") Long id);

    @Query("SELECT NEW com.vz.backend.business.dto.NodeDto(flow.prevId, flow.next.type, flow.next.allowMultiple, flow.next.bpmn.active) FROM NodeModel2 flow WHERE flow.prevId IN (:prevIds)")
    List<NodeDto> findAutoNextById(@Param("prevIds") List<Long> id);

    @Nullable
    @Query("SELECT new com.vz.backend.business.dto.BpmnActiveWrapper(n.bpmn.active, n.bpmn.name) FROM NodeModel2 n WHERE n.id=:nodeId")
    BpmnActiveWrapper wrapBpmnByNodeId(Long nodeId);

    @NonNull
    @Query(" select distinct new com.vz.backend.business.dto.NodeDto(flow.next.id, flow.next.name, flow.next.type, flow.next.allowMultiple, flow.name, flow.bpmn.name, bp.orgId, bp.org.name) "
            + "FROM NodeModel2 flow INNER JOIN BpmnModel2 bp on bp.id=flow.bpmnId where flow.bpmn.clientId=:clientId AND flow.bpmn.active is TRUE AND "
            + "flow.bpmn.typeDocument=:typeDocument AND flow.bpmn.orgId in :orgIds AND flow.id IN ("
            + "select flow.id FROM NodeModel2 flow inner join Condition c on flow.prevId=c.nodeId where "
            + "flow.prev.type IN ('task', 'startEvent') AND ("
            + "c.userId=:userId OR  (c.positionId is null AND :userLead is TRUE AND c.orgId=:userOrgId) OR "
            + "((c.orgId is NULL OR c.orgId in :orgIds) AND :userPosId = c.positionId)))")
    List<NodeDto> nextStartNodeOfBpmn(Long userId, boolean userLead, Long userPosId, Long userOrgId, Long clientId,
                                      List<Long> orgIds, TYPE_DOCUMENT typeDocument);

    @NonNull
    @Query("select new com.vz.backend.business.dto.NodeDto(flow.next.id, flow.next.name, flow.next.type, flow.next.allowMultiple, flow.name, flow.bpmn.name, bp.orgId, bp.org.name) "
            + "FROM NodeModel2 flow INNER JOIN BpmnModel2 bp on bp.id=flow.bpmnId inner join NodeModel2 n on flow.prevId=n.id "
            + "where flow.bpmn.clientId=:clientId AND flow.bpmn.active is TRUE AND flow.bpmn.typeDocument=:typeDocument AND flow.bpmn.orgId IN :orgIds AND "
            + "n.type='startEvent'")
    List<NodeDto> simpleNextStartNodeOfBpmn(Long clientId, List<Long> orgIds, TYPE_DOCUMENT typeDocument);

    @Query("SELECT c.nodeId FROM Condition c WHERE c.active is TRUE AND c.nodeId in :setNode AND c.allowConfig is NOT FALSE AND ("
            + "c.userId=:userId OR (c.positionId is NULL AND c.orgId=:org) OR (c.positionId=:position AND c.orgId in :setOrg))")
    Set<Long> validNode(Set<Long> setNode, Long org, Long position, Long userId, List<Long> setOrg);

    @Query("SELECT c.nodeId FROM Condition c WHERE c.active is TRUE AND c.nodeId = :setNode AND c.allowConfig is NOT FALSE AND ("
            + "c.userId=:userId OR (c.positionId is NULL AND c.orgId=:org) OR (c.positionId=:position AND c.orgId in :setOrg))")
    Set<Long> validNode(Long setNode, Long org, Long position, Long userId, List<Long> setOrg);

    @Query("SELECT n.importDocBook FROM NodeModel2 n WHERE n.id = :nodeId")
    Boolean getImportDocBookByNodeId(Long nodeId);

    @Query("SELECT n.reviewRequired FROM NodeModel2 n WHERE n.id = :nodeId")
    Boolean getReviewRequiredByNodeId(Long nodeId);

    @Query("SELECT n.forceCloseBranch FROM NodeModel2 n WHERE n.id = :nodeId")
    Boolean getCloseBranchByNodeId(Long nodeId);

    @Query("SELECT NEW com.vz.backend.business.dto.NodeDto(flow.id, flow.reviewRequired, flow.forceCloseBranch) FROM NodeModel2 flow WHERE flow.id IN (:ids)")
    List<NodeDto> getDataByNodeId(@Param("ids") List<Long> id);

    @Query("SELECT c FROM Condition c WHERE c.active IS TRUE AND c.nodeId = :nodeId AND c.security = TRUE")
    List<Condition> getConditionByNodeIdAndSecurityTrue(Long nodeId);

    @Query("SELECT c.forceCloseBranch FROM NodeModel2 c WHERE c.active IS TRUE AND c.id = :nodeId")
    Boolean isForceCloseBranchByNodeId(Long nodeId);

    @Query("SELECT c FROM NodeModel2 c WHERE c.active IS TRUE AND c.id = :nodeId and c.clientId =:clientId")
    NodeModel2 getByIdAndClientId(Long nodeId, Long clientId);

    @Query("select count(1) > 0 from NodeModel2 nodeModel where nodeModel.id in ( select node.next.id from NodeModel2 node where node.prevId in (:id)) and type = 'endEvent'")
    Boolean isPreviousNextNode(Long id);

    @Query("SELECT condition.positionId FROM Condition condition where condition.nodeId = :nodeId")
    List<Long> getListPositionInConditionByNodeId(Long nodeId);

    @Query("SELECT c FROM Condition c WHERE c.active IS TRUE AND c.nodeId = :nodeId")
    List<Condition> getConditionByNodeId(Long nodeId);

    @Query("SELECT node.id FROM BpmnModel2 bpmn2 JOIN NodeModel2 node ON bpmn2.id = node.bpmnId WHERE node.clientId = :clientId and node.active = :active and bpmn2.orgId = :orgId and node.type = 'endEvent' and bpmn2.typeDocument = 'INCOMING'")
    Long getLastNodeByOrgId(Long clientId, Boolean active, Long orgId);
}
