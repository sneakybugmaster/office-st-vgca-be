package com.vz.backend.business.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.domain.DocumentOutTracking;
import com.vz.backend.core.config.DocumentInTrackingEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class FollowDto {
	private int no;
	private String org;
	private String position;
	private String fullName;
	private String action;
	private String category;
	private Date createDate;

	@JsonIgnore
	private Long userId;

	@JsonIgnore
	private DocumentInTrackingEnum actionEnum;
	
	private String transferer;

	public FollowDto(int no, String org, String fullName, String action, String category, Date createDate) {
		this.no = no;
		this.org = org;
		this.fullName = fullName;
		this.action = action;
		this.category = category;
		this.createDate = createDate;
	}

	public FollowDto(Long userId, String org, String position, String fullName, DocumentInTrackingEnum actionEnum,
			String category, Date createDate) {
		this.userId = userId;
		this.org = org;
		this.fullName = fullName;
		this.actionEnum = actionEnum;
		this.category = category;
		this.createDate = createDate;
		this.position = position;
		this.action = actionEnum.getName();
	}
	
	public FollowDto(DocumentOutTracking d) {
		this.setAction(d.getAction().getName());
		this.createDate = d.getUpdateDate() != null ? d.getUpdateDate() : d.getCreateDate();
		this.setCategory(d.getDoc().getDocType().getName());
		if (d.getHandlerId() != null) {
			this.setFullName(d.getHandler().getFullName());
			this.setPosition(d.getHandler().getPositionModel().getName());
			this.setOrg(d.getHandler().getOrgModel().getName());
		} else {
			this.setFullName(d.getFromUser().getFullName());
			this.setPosition(d.getFromUser().getPositionModel().getName());
			this.setOrg(d.getFromUser().getOrgModel().getName());
		}
	}
}
