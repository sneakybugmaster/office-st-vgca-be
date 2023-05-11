package com.vz.backend.business.dto.document;

import com.vz.backend.business.config.DocInternalApproveTypeEnum;
import com.vz.backend.business.config.DocInternalHandleEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
public class DocInternalReceiverDto {
	private Long id;
	private DocInternalApproveTypeEnum type;
	private Long userId;
	private String userName;
	private Long orgId;
	private String orgName;
	private DocInternalHandleEnum handleStatus;
	private List<DocInCommentDto> listCmt;

	public DocInternalReceiverDto(Long id, DocInternalApproveTypeEnum type, Long userId, String userName, Long orgId, String orgName, DocInternalHandleEnum handleStatus) {
		super();
		this.id = id;
		this.type = type;
		this.userId = userId;
		this.userName = userName;
		this.orgId = orgId;
		this.orgName = orgName;
		this.handleStatus = handleStatus;
	}
}
