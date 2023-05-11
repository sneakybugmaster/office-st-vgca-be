package com.vz.backend.core.dto;

import com.vz.backend.core.domain.Category;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class UserTreeDto {
	private Long id;
	private Long orgId;
	private Boolean lead;
	private Long positionId;
	private String positionName;
	private Integer positionOrder;
	private String fullName;
	private String userName;
	private boolean isDirectionAuthority;

	public UserTreeDto(Long id, Long orgId, Category position, String fullName, String userName, Boolean lead) {
		this.id = id;
		this.orgId = orgId;
		this.positionId = position.getId();
		// TODO: get this info by one query
		this.positionName = position.getName();
		this.positionOrder = position.getOrder();
		this.fullName = fullName;
		if (this.fullName != null) {
			this.fullName = fullName.trim();
		}
		this.userName = userName;
		if (this.userName != null) {
			this.userName = userName.trim();
		}
		this.lead = lead;
	}

	public UserTreeDto(Long id, Long orgId, Boolean lead, Long positionId, String positionName, Integer positionOrder,
			String fullName, String userName) {
		this.id = id;
		this.orgId = orgId;
		this.lead = lead;
		this.positionId = positionId;
		this.positionName = positionName;
		this.positionOrder = positionOrder;
		this.fullName = fullName;
		this.userName = userName;
	}
}
