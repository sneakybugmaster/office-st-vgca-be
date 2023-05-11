package com.vz.backend.business.dto;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.AttachmentDelegate;

import lombok.Getter;
import lombok.Setter;

@Getter
public class DelegateDto {
	private Long id;
	private String numberOrSign;
	private Long fromUserId;
	private String fromUserName;
	private Long toUserId;
	private String toUserName;
	private Date startDate;
	private Date endDate;
	private Boolean active;
	@Setter
	private List<AttachmentDelegate> attachments;

	public DelegateDto(Long id, String numberOrSign, Long fromUserId, String fromUserName, Long toUserId,
			String toUserName, Date startDate, Date endDate) {
		super();
		this.id = id;
		this.numberOrSign = numberOrSign;
		this.fromUserId = fromUserId;
		this.fromUserName = fromUserName;
		this.toUserId = toUserId;
		this.toUserName = toUserName;
		this.startDate = startDate;
		this.endDate = endDate;
	}
	public DelegateDto(Long id, String numberOrSign, Long fromUserId, String fromUserName, Long toUserId,
					   String toUserName, Date startDate, Date endDate, Boolean active) {
		super();
		this.id = id;
		this.numberOrSign = numberOrSign;
		this.fromUserId = fromUserId;
		this.fromUserName = fromUserName;
		this.toUserId = toUserId;
		this.toUserName = toUserName;
		this.startDate = startDate;
		this.endDate = endDate;
		this.active = active;
	}
}
