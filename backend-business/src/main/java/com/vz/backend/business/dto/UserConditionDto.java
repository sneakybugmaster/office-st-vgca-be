package com.vz.backend.business.dto;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.domain.DocumentInProcess;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;

import lombok.Getter;
import lombok.Setter;

// TODO: ext UserTreeDto
@Getter
@Setter
public class UserConditionDto extends UserPositionBase {
	private static final long serialVersionUID = 1L;

	private boolean lead; // defaults
	private boolean breadth;
	private boolean siblings;
	private Long org;
	private Long position;

	// Config
	@JsonIgnore
	private Long orgId;
	@JsonIgnore
	private Long positionId;
	@JsonIgnore
	private Long userId;
	@JsonIgnore
	private boolean forceSameOrg;
	List<UserDelegateDto> delegateUsers;
	private String orgName;

	public UserConditionDto(Long id, Boolean isDefault, Boolean breadth, Boolean isSiblings, Long org,
			Long position,
			String positionName,
			Integer positionOrder,
			String fullName, String userName, Long orgId, Long positionId, Long userId) {
		super(id, userName, fullName, positionName, positionOrder);
		this.lead = Boolean.TRUE.equals(isDefault);
		this.breadth = Boolean.TRUE.equals(breadth);
		this.siblings = Boolean.TRUE.equals(isSiblings);
		this.org = org;
		this.position = position;

		this.orgId = orgId;
		this.positionId = positionId;
		this.userId = userId;
	}

	public UserConditionDto(Long id, Boolean isDefault, Boolean breadth, Boolean isSiblings, Long org, Long position,
			String positionName, Integer positionOrder, String fullName, String userName, Long orgId, Long positionId,
			Long userId, Boolean forceSameOrg, String orgName, Category secondPosition) {

		this(id, isDefault, breadth, isSiblings, org, position, positionName, positionOrder, fullName, userName, orgId,
				positionId, userId);
		if (secondPosition != null) {
			this.lead = Boolean.TRUE.equals(isDefault) || secondPosition.getIsDefault();
			this.breadth = Boolean.TRUE.equals(breadth) || secondPosition.getIsBreadth();
			this.siblings = Boolean.TRUE.equals(isSiblings) || secondPosition.getIsSiblings();
			this.positionId = secondPosition.getId();
		}
		this.forceSameOrg = Boolean.TRUE.equals(forceSameOrg);
		this.orgName = orgName;

	}

	public static UserConditionDto set(DocumentInProcess p, boolean delegate) {
		User user = delegate && p.getDelegater() != null ? p.getDelegater() : p.getToUsers();
		Long userId = user.getId();
		Category position = user.getPositionModel();
		return new UserConditionDto(userId, position.getIsDefault(), position.getIsBreadth(), position.getIsSiblings(),
				user.getOrg(), position.getId(), position.getName(), position.getOrder(), user.getFullName(),
				user.getUserName(), user.getOrg(), user.getPosition(), userId);
	}
	
	public boolean invalid() {
		return this.userId == null && this.orgId != null && this.positionId != null;
	}
	
	public UserConditionDto(Long id, Boolean isDefault, Boolean breadth, Boolean isSiblings, Long org, Long position,
			String positionName, Integer positionOrder, String fullName, String userName, String orgName) {
		super(id, userName, fullName, positionName, positionOrder);
		this.lead = Boolean.TRUE.equals(isDefault);
		this.breadth = Boolean.TRUE.equals(breadth);
		this.siblings = Boolean.TRUE.equals(isSiblings);
		this.org = org;
		this.position = position;
		this.orgName = orgName;
	}
}
