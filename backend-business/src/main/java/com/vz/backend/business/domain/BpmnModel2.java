package com.vz.backend.business.domain;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.util.NodeType;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Entity
@Data
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "BPMN", schema = "vz", indexes = {
		@Index(name = "idx_bpmnmodel2_id_org_id", columnList = "id, org_id")
}, uniqueConstraints = {
		@UniqueConstraint(columnNames = {"client_id", "name"})
})
@JsonIgnoreProperties(value = { "id" }, allowGetters = true)
public class BpmnModel2 extends BaseModel {
	private static final long serialVersionUID = 1L;
	public static final String PROCESS_TAG = "process";

	public static final String CONTENT_DEFAULT = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<definitions xmlns=\"http://www.omg.org/spec/BPMN/20100524/MODEL\" xmlns:bpmndi=\"http://www.omg.org/spec/BPMN/20100524/DI\" xmlns:omgdc=\"http://www.omg.org/spec/DD/20100524/DC\" xmlns:omgdi=\"http://www.omg.org/spec/DD/20100524/DI\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" targetNamespace=\"\" xsi:schemaLocation=\"http://www.omg.org/spec/BPMN/20100524/MODEL http://www.omg.org/spec/BPMN/2.0/20100501/BPMN20.xsd\">\n  <process id=\"Process_0l7d3rj\">\n    <startEvent id=\"Event_000a9fg\">\n      <outgoing>Flow_1x3osg7</outgoing>\n    </startEvent>\n    <task id=\"Activity_0ictzx5\">\n      <incoming>Flow_1x3osg7</incoming>\n      <outgoing>Flow_1qaycfp</outgoing>\n    </task>\n    <sequenceFlow id=\"Flow_1x3osg7\" sourceRef=\"Event_000a9fg\" targetRef=\"Activity_0ictzx5\" />\n    <endEvent id=\"Event_0c6fm7l\">\n      <incoming>Flow_1qaycfp</incoming>\n    </endEvent>\n    <sequenceFlow id=\"Flow_1qaycfp\" sourceRef=\"Activity_0ictzx5\" targetRef=\"Event_0c6fm7l\" />\n  </process>\n  <bpmndi:BPMNDiagram id=\"sid-74620812-92c4-44e5-949c-aa47393d3830\">\n    <bpmndi:BPMNPlane id=\"sid-cdcae759-2af7-4a6d-bd02-53f3352a731d\" bpmnElement=\"Process_0l7d3rj\">\n      <bpmndi:BPMNEdge id=\"Flow_1x3osg7_di\" bpmnElement=\"Flow_1x3osg7\">\n        <omgdi:waypoint x=\"48\" y=\"240\" />\n        <omgdi:waypoint x=\"100\" y=\"240\" />\n      </bpmndi:BPMNEdge>\n      <bpmndi:BPMNEdge id=\"Flow_1qaycfp_di\" bpmnElement=\"Flow_1qaycfp\">\n        <omgdi:waypoint x=\"200\" y=\"240\" />\n        <omgdi:waypoint x=\"252\" y=\"240\" />\n      </bpmndi:BPMNEdge>\n      <bpmndi:BPMNShape id=\"Event_000a9fg_di\" bpmnElement=\"Event_000a9fg\">\n        <omgdc:Bounds x=\"12\" y=\"222\" width=\"36\" height=\"36\" />\n      </bpmndi:BPMNShape>\n      <bpmndi:BPMNShape id=\"Activity_0ictzx5_di\" bpmnElement=\"Activity_0ictzx5\">\n        <omgdc:Bounds x=\"100\" y=\"200\" width=\"100\" height=\"80\" />\n      </bpmndi:BPMNShape>\n      <bpmndi:BPMNShape id=\"Event_0c6fm7l_di\" bpmnElement=\"Event_0c6fm7l\">\n        <omgdc:Bounds x=\"252\" y=\"222\" width=\"36\" height=\"36\" />\n      </bpmndi:BPMNShape>\n    </bpmndi:BPMNPlane>\n    <bpmndi:BPMNLabelStyle id=\"sid-e0502d32-f8d1-41cf-9c4a-cbb49fecf581\">\n      <omgdc:Font name=\"Arial\" size=\"11\" isBold=\"false\" isItalic=\"false\" isUnderline=\"false\" isStrikeThrough=\"false\" />\n    </bpmndi:BPMNLabelStyle>\n    <bpmndi:BPMNLabelStyle id=\"sid-84cb49fd-2f7c-44fb-8950-83c3fa153d3b\">\n      <omgdc:Font name=\"Arial\" size=\"12\" isBold=\"false\" isItalic=\"false\" isUnderline=\"false\" isStrikeThrough=\"false\" />\n    </bpmndi:BPMNLabelStyle>\n  </bpmndi:BPMNDiagram>\n</definitions>";

	public enum TYPE_DOCUMENT {
		INCOMING, OUTGOING, ASSIGN, EXAM_FOR_OTHERS, WORD_EDITOR, CABINET_DRAFT, INTERNAL_INCOMING, INTERNAL_ISSUED_INCOMING
	}

	@Column(name = "[name]", nullable = false)
	private String name;

	public void setName(String n) {
		if (n == null) {
			throw new RestExceptionHandler("Can't set `name` = null");
		}
		this.name = n.trim();
		if (this.name.length() == 0) {
			throw new RestExceptionHandler("Can't set empty name");
		}
	}

	@ToString.Exclude
	@Column(name = "content", length = 100000, nullable = false)
	private String content;

	@Transient
	private List<Long> startNodeIds;

	@Column(name = "org_id")
	private Long orgId;
	@ManyToOne
	@JoinColumn(name = "org_id", updatable = false, insertable = false)
	private Organization org;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "bpmn", cascade = { CascadeType.ALL }, orphanRemoval = true)
//	@JsonIgnore
	private List<NodeModel2> nodes;

	@Transient
	@JsonIgnore
	private boolean showNode = false;

	@Column(name = "type_document", nullable = false)
	@Enumerated(EnumType.STRING)
	private TYPE_DOCUMENT typeDocument;

	public void setContent(String content) {
		if (content != null) {
			this.content = content.replaceAll("\\n\\s*", "");
		}
	}

	public List<NodeModel2> getNodes() {
		List<NodeModel2> result = new ArrayList<>();
		if (!this.showNode) {
			return result;
		}
		if (this.nodes == null) {
			return result;
		}
		for (NodeModel2 node : this.nodes) {
			if (node != null && (NodeType.START_EVENT.equals(node.getType()) || NodeType.TASK.equals(node.getType())
					|| NodeType.END_EVENT.equals(node.getType()))) {
				result.add(node);
			}
		}
		return result;
	}

	public List<NodeModel2> fullNodes() {
		return this.nodes;
	}
}
