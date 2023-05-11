package com.vz.backend.business.dto.document;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.vz.backend.business.config.DocInternalApproveStatusEnum;
import com.vz.backend.business.config.DocInternalApproveTypeEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class ApproverDto {
	private Long id;
	private DocInternalApproveTypeEnum type;
	private DocInternalApproveStatusEnum handleStatus;
	private Long userId;
	private String userFullName;
	private String positionName;
	private Long orgId;
	private String orgName;
	private String comment;
	private Date createDate;
	private List<DocInCommentDto> listCmt;
	
	public ApproverDto(Long id, DocInternalApproveTypeEnum type, DocInternalApproveStatusEnum handleStatus, Long userId, String userFullName, String positionName, Long orgId, String orgName, String comment) {
		super();
		this.id = id;
		this.type = type;
		this.handleStatus = handleStatus;
		this.userId = userId;
		this.userFullName = userFullName;
		this.positionName = positionName;
		this.orgId = orgId;
		this.orgName = orgName;
		this.comment = comment;
		this.listCmt = new ArrayList<>();
	}

	public ApproverDto(Long id, Long userId, String userFullName, String positionName, String comment) {
		super();
		this.id = id;
		this.userId = userId;
		this.userFullName = userFullName;
		this.positionName = positionName;
		this.comment = comment;
		this.listCmt = new ArrayList<>();
	}
	
	public ApproverDto(Long id, Long userId, String userFullName, String positionName, String comment, Date createDate) {
		this(id, userId, userFullName, positionName, comment);
		this.createDate = createDate;
	}
}
