package com.vz.backend.business.dto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.vz.backend.business.domain.DocumentInProcess;
import com.vz.backend.core.common.BussinessCommon;

import lombok.Data;

@Data
public class DocumentOutProcessDto {
	@JsonProperty(value = "pId")
	private Long pId;
	private Long docId;
	private Long userId;
	private Long nodeId;
	private String fullName;
	private String positionName;
	private String orgName;
	private int step;

	public DocumentOutProcessDto(Long docId, Long userId, Long nodeId, String fullName, String positionName,
			String orgName) {
		super();
		this.docId = docId;
		this.userId = userId;
		this.nodeId = nodeId;
		this.fullName = fullName;
		this.positionName = positionName;
		this.orgName = orgName;
	}
	
	public DocumentOutProcessDto(DocumentInProcess p) {
		super();
		this.docId = p.getDocId();
		this.userId = p.getToUser();
		this.nodeId = p.getNode();
		this.fullName = p.getToUsers().getFullName();
		this.positionName = p.getToUsers().getPositionModel().getName();
		this.orgName = p.getToUsers().getOrgModel().getName();
		this.pId = p.getId();
		this.step = p.getStep();
	}
	
	public static List<DocumentOutProcessDto> convert(List<DocumentInProcess> pList, boolean noContainCurUserId) {
		if (pList.isEmpty())
			return Collections.emptyList();
		List<DocumentOutProcessDto> rs = new ArrayList<>();
		Long userId = BussinessCommon.getUserId();
		for (DocumentInProcess i : pList) {
			if(noContainCurUserId && userId.equals(i.getToUser())) {
				continue;
			}
			rs.add(new DocumentOutProcessDto(i));
		}
		return rs;
	}
}
