package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@AllArgsConstructor
@Data
public class UserInfoDto {
	private Long id;
	private String fullName;
	private String userName;
	private Long positionId;
	private String positionName;
	private Long orgId;
	private String orgName;
	private boolean lead;
	private int orderPosition;

	public UserInfoDto(Long positionId) {
		this.positionId = positionId;
	}
	
	public UserInfoDto(Long id, String fullName, String userName, boolean lead, String title, Long positionId,
			Long orgId) {
		this.id = id;
		this.fullName = fullName;
		this.userName = userName;
		this.lead = lead;
		this.positionId = positionId;
		this.orgId = orgId;
	}

	public UserInfoDto(Long id, String fullName, String userName) {
		this.id = id;
		this.fullName = fullName;
		this.userName = userName;
	}

	public UserInfoDto(Long id, Long orgId, String fullName, String orgName) {
		this.id = id;
		this.orgId = orgId;
		this.fullName = fullName;
		this.orgName = orgName;
	}

	public UserInfoDto(Long id, String fullName, String positionName, String orgName) {
		super();
		this.id = id;
		this.fullName = fullName;
		this.positionName = positionName;
		this.orgName = orgName;
	}

	public UserInfoDto(Long id, String fullName, String positionName, Long orgId) {
		super();
		this.id = id;
		this.fullName = fullName;
		this.positionName = positionName;
		this.orgId = orgId;
	}
	
	public UserInfoDto(Long id, String fullName, String positionName, Long orgId, String orgName, boolean lead) {
		super();
		this.id = id;
		this.fullName = fullName;
		this.positionName = positionName;
		this.orgId = orgId;
		this.orgName = orgName;
		this.lead = lead;
	}

	public UserInfoDto(Long id, String fullName, String positionName, String orgName, int orderPosition) {
		super();
		this.id = id;
		this.fullName = fullName;
		this.positionName = positionName;
		this.orgName = orgName;
		this.orderPosition = orderPosition;
	}


}
