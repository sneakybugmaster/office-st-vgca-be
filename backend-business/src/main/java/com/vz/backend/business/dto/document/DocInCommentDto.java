package com.vz.backend.business.dto.document;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.config.DocInternalApproveStatusEnum;
import com.vz.backend.business.config.DocumentCommentTypeEnum;
import com.vz.backend.business.domain.AttachmentComment;
import com.vz.backend.business.domain.DocumentComment;
import com.vz.backend.business.domain.documentInternal.DocInternalAttach;
import com.vz.backend.core.common.BussinessCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DocInCommentDto extends CommentDto {
	private List<AttachmentComment> attachments;
	private List<DocInternalAttach> internalAttachments;
	private boolean editable;
	private String cmtContent;
	private boolean isTransfer;
	private DocumentCommentTypeEnum type;
	private String typeName;

	public DocInCommentDto(DocumentComment dc) {
		this.setId(dc.getId());
		this.setUserFullName(dc.getUserFullName());
		this.setUserPosition(dc.getUserPosition());
		this.setComment(dc.getComment());
		this.setIsToken(dc.getIsToken());
		this.setCreateDate(dc.getCreateDate());
		if(dc.getCreateBy().equals(BussinessCommon.getUserId())) {
			editable = true;
		}
		if(dc.getIsTransfer() != null) {
			this.setTransfer(dc.getIsTransfer());
		}
		this.cmtContent = dc.getCmtContent();
		this.attachments = dc.getAttachments();
		this.type = dc.getType();
		this.typeName = dc.getType() == null ? "" : dc.getType().getName();
	}
	
	public DocInCommentDto(CommentDto dto) {
		this.setId(dto.getId());
		this.setUserFullName(dto.getUserFullName());
		this.setUserPosition(dto.getUserPosition());
		this.setComment(dto.getComment());
		this.setIsToken(dto.getIsToken());
		this.setCreateDate(dto.getCreateDate());
	}

	public DocInCommentDto(Long id, Long approveId, String userFullName, String userPosition, String comment, Date createDate, DocInternalApproveStatusEnum handleStatus) {
		super(id, approveId, userFullName, userPosition, comment, createDate, handleStatus);
	}
}
