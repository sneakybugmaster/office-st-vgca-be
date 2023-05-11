package com.vz.backend.business.dto.hstl;

import java.math.BigInteger;

import com.vz.backend.business.config.HsFolderStatusEnum;

import lombok.Getter;

@Getter
public class ReportProcessDto {
	private Long fId;
	private Long fromUserId;
	private Long toUserId;
	private Long toOrgId;
	private HsFolderStatusEnum status;
	private String type;

	public ReportProcessDto(Object fId, Object fromUserId, Object toUserId, Object toOrgId, Object status, Object type) {
		super();
		this.fId = ((BigInteger) fId).longValue();
		this.fromUserId = fromUserId != null ? ((BigInteger) fromUserId).longValue() : null;
		this.toUserId = toUserId != null ? ((BigInteger) toUserId).longValue() : null;
		this.toOrgId = toOrgId != null ? ((BigInteger) toOrgId).longValue() : null;
		this.status = status != null ? HsFolderStatusEnum.getEnumValue(status.toString()) : null;
		this.type = type.toString() ;
	}
}
