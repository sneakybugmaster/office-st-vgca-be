package com.vz.backend.business.dto.document;

import java.util.Date;

import com.vz.backend.business.config.DocInternalApproveStatusEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class CommentDto {
	private Long id;
	private Long approveId;
	private String userFullName;
	private String userPosition;
	private String comment;
	private DocInternalApproveStatusEnum handleStatus;
	private Boolean isToken;
	private Date createDate;

	public String getHandleStatusName() {
		return handleStatus == null ? "" : handleStatus.getName();
	}
	
	public CommentDto(Long id, String userFullName, String userPosition, String comment, Boolean isToken, Date createDate) {
		this.id = id;
		this.userFullName = userFullName;
		this.userPosition = userPosition;
		this.comment = comment;
		this.isToken = isToken;
		this.createDate = createDate;
	}
	
	public CommentDto(Long id, String userFullName, String userPosition, String comment, Boolean isToken, Date createDate, DocInternalApproveStatusEnum handleStatus) {
		this(id, userFullName, userPosition, comment, isToken, createDate);
		this.handleStatus = handleStatus;
	}

	public CommentDto(Long id, Long approveId, String userFullName, String userPosition, String comment, Date createDate) {
		super();
		this.id = id;
		this.approveId = approveId;
		this.userFullName = userFullName;
		this.userPosition = userPosition;
		this.comment = comment;
		this.createDate = createDate;
	}
	
	public CommentDto(Long id, Long approveId, String userFullName, String userPosition, String comment, Date createDate, DocInternalApproveStatusEnum handleStatus) {
		this(id, approveId, userFullName, userPosition, comment, createDate);
		this.handleStatus = handleStatus;
	}
}
