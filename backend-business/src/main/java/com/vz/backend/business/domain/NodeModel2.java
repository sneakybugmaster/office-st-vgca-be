package com.vz.backend.business.domain;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.beans.BeanUtils;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.util.NodeType;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.util.StringUtils;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Entity
@Data
@EqualsAndHashCode(callSuper = false)
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "NODE", schema = "vz")
@JsonIgnoreProperties({ "active", "type", "clientId", "createDate", "updateDate", "createBy", "updateBy",
		"hibernateLazyInitializer" })
public class NodeModel2 extends BaseModel {

	private static final long serialVersionUID = 1L;

	public static final String IDENT_TAG = "id";

	private String ident;

	@Transient
	@JsonIgnore
	private String prevStr;

	@Transient
	@JsonIgnore
	private String nextStr;

	@OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@JoinColumn(name = "next_id")
	@JsonIgnore
	@ToString.Exclude
	private NodeModel2 next;

	@OneToOne(cascade = CascadeType.ALL, fetch = FetchType.LAZY)
	@JoinColumn(name = "prev_id")
	@JsonIgnore
	@ToString.Exclude
	private NodeModel2 prev;
	@Column(name = "prev_id", updatable = false, insertable = false)
	@JsonIgnore
	private Long prevId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "bpmn_id")
	@JsonIgnore
	@ToString.Exclude
	private BpmnModel2 bpmn;
	@JsonIgnore
	@ToString.Exclude
	@Column(name = "bpmn_id", updatable = false, insertable = false)
	private Long bpmnId;
	private Boolean allowMultiple;

	public void setAllowMultiple(Boolean allowMultiple) {
		if (allowMultiple == null) {
			allowMultiple = false;
		}
		this.allowMultiple = allowMultiple;
	}
	private Boolean reviewRequired;

	public void setReviewRequired(Boolean reviewRequired) {
		if (reviewRequired == null) {
			reviewRequired = false;
		}
		this.reviewRequired = reviewRequired;
	}

	private Boolean importDocBook;

	/**
	 * CR : #2769 (15/3/2022)
	 * Allow the current node to complete process and close the branch processes
	 */
	private Boolean forceCloseBranch;

	public void setImportDocBook(Boolean importDocBook) {
		if (importDocBook == null) {
			importDocBook = false;
		}
		this.importDocBook = importDocBook;
	}

	public NodeModel2(NodeModel2 node, String name) {
		BeanUtils.copyProperties(node, this);
		if (!StringUtils.isNullOrEmpty(name)) {
			this.name = name;
		}
	}

	public NodeModel2(NodeModel2 node, String name, String bpmnName) {
		this(node, name);
		this.name = bpmnName + " - " + this.name;
	}

	public void from(NodeModel2 another) {
		this.prevStr = another.prevStr;
		this.nextStr = another.nextStr;
		this.bpmn = another.bpmn;
		this.type = another.type;
		this.name = another.name;
	}

	@Column(name = "type")
	private String type;

	@Column(name = "[name]")
	private String name;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "node", cascade = { CascadeType.ALL }, orphanRemoval = true)
	private List<Condition> conditions;

	public boolean isLastNode() {
		return NodeType.END_EVENT.equals(this.type);
	}

	public void init(Element element) {

		this.ident = element.getAttribute(IDENT_TAG);
		this.type = element.getNodeName();
		this.name = element.getAttribute("name");

		switch (this.type) {
		case "sequenceFlow":
			this.prevStr = element.getAttribute("sourceRef");
			this.nextStr = element.getAttribute("targetRef");
			break;
		case "laneSet":
		case "exclusiveGateway":
			break;

		// startEvent, endEvent, task
		default:
			NodeList outgoing = element.getElementsByTagName("outgoing");
			if (outgoing.getLength() == 1) {
				this.nextStr = outgoing.item(0).getTextContent();
			}

			NodeList incoming = element.getElementsByTagName("incoming");
			if (incoming.getLength() == 1) {
				this.prevStr = incoming.item(0).getTextContent();
			}
		}
	}

	public void initConditions(NodeModel2 node) {
		this.setAllowMultiple(node.getAllowMultiple());
		this.setReviewRequired(node.getReviewRequired());
		this.setImportDocBook(node.getImportDocBook());
		this.setForceCloseBranch(node.getForceCloseBranch());
		List<Condition> nodeConditions = node.getConditions();
		if (nodeConditions == null) {
			nodeConditions = new ArrayList<>();
		}
		Iterator<Condition> iterator = nodeConditions.iterator();
		while (iterator.hasNext()) {
			Condition condition = iterator.next();
			if (!condition.valid() || NodeType.START_EVENT.equals(this.type) && condition.getPositionId() == null) {
				iterator.remove();
			}
		}
		if (this.conditions == null) {
			this.conditions = nodeConditions;
			for (Condition condition : this.conditions) {
				condition.setNode(this);
			}
			return;
		}

		int iCurr = 0;
		int iNew = 0;
		int currSize = this.conditions.size();
		int newSize = nodeConditions.size();
		int max = Math.max(currSize, newSize);
		for (int i = 0; i < max; ++i) {
			if (iCurr >= currSize) {
				Condition condition = nodeConditions.get(iNew++); // add other node
				condition.setNode(this);
				this.conditions.add(condition);
				continue;
			}
			if (iNew >= newSize) {
				this.conditions.remove(iCurr);
				continue;
			}
			Condition condition = nodeConditions.get(iNew++); // move next node
			Condition conditionCurr = this.conditions.get(iCurr++); // replace node success
			condition.setNode(this);
			conditionCurr.clone(condition);
		}
	}

}
