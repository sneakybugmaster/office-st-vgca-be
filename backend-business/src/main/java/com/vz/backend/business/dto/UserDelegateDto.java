package com.vz.backend.business.dto;

import java.util.Date;
import java.util.List;

import lombok.Data;

@Data
public class UserDelegateDto extends UserPositionBase {
	private static final long serialVersionUID = 1L;

	private Long fromId;
	private Long delegateId;
	private Date createDate;
	private Date endDate;
	List<AttachmentDelegateDto> attachments;

	public UserDelegateDto(Long delegateId, Date createDate, Date endDate, Long fromId, Long id, String userName, String fullName,
			String positionName, Integer positionOrder) {
		super(id, userName, fullName, positionName, positionOrder);
		this.fromId = fromId;
		this.delegateId = delegateId;
		this.endDate = endDate;
		this.createDate = createDate;
	}
}
