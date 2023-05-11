package com.vz.backend.business.dto.outsideconnect;

import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class SentDataDto {
	private Long outsideId;
	private Long orgId;
	private String orgName;
	private List<Long> userIds;

	public SentDataDto(Long outsideId, Long orgId, String orgName) {
		super();
		this.outsideId = outsideId;
		this.orgId = orgId;
		this.orgName = orgName;
	}

	public SentDataDto(Long orgId, String orgName, List<Long> userIds) {
		super();
		this.orgId = orgId;
		this.orgName = orgName;
		this.userIds = userIds;
	}
}
