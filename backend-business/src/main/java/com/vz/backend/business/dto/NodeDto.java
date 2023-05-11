package com.vz.backend.business.dto;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.util.NodeType;
import com.vz.backend.util.StringUtils;

import lombok.Getter;
import lombok.Setter;

@Setter
@Getter
public class NodeDto {
	private Long id;
	private Long orgId;
	private String name;
	private String orgName;
	private boolean lastNode;
	private boolean allowMultiple;
	@JsonIgnore
	private boolean bpmnActive;
	@JsonIgnore
	private boolean reviewRequired;
	@JsonIgnore
	private Boolean closeBranch;
	
	public NodeDto(Long id, String name, String type, Boolean allowMultiple, String flowName, Long orgId, String orgName) {
		this.id = id;
		this.orgId = orgId;
		this.orgName = orgName;
		if (StringUtils.isNullOrEmpty(flowName)) {
			this.name = name;
		} else {
			this.name = flowName;
		}
		this.lastNode = NodeType.END_EVENT.equals(type);
		if (allowMultiple == null) {
			allowMultiple = false;
		}
		this.allowMultiple = allowMultiple;
	}
	
	public NodeDto(Long id, String name, String type, Boolean allowMultiple, String flowName, String bpmnName, Long orgId, String orgName) {
		this(id, name, type, allowMultiple, flowName, orgId, orgName);
		this.name = bpmnName + " - " + this.name;
	}

	public NodeDto(Long id, String type, boolean allowMultiple, boolean bpmnActive) {
		super();
		this.id = id;
		this.bpmnActive = bpmnActive;
		this.allowMultiple = allowMultiple;
		this.lastNode = NodeType.END_EVENT.equals(type);
	}
	
	public NodeDto(Long id, boolean reviewRequired, Boolean closeBranch) {
		super();
		this.id = id;
		this.reviewRequired = reviewRequired;
		this.closeBranch = closeBranch;
	}
}
