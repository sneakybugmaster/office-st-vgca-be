package com.vz.backend.business.dto.document;

import java.util.List;

import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.DocumentOutComment;
import com.vz.backend.core.common.BussinessCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class DocOutCommentDto extends CommentDto {
	private List<DocumentOutAttachment> attachments;
	private boolean editable;

	public DocOutCommentDto(DocumentOutComment dc) {
		this.setId(dc.getId());
		this.setUserFullName(dc.getUser().getFullName());
		this.setUserPosition(dc.getUser().getPositionModel().getName());
		this.setComment(dc.getComment());
		this.setIsToken(dc.getIsToken());
		this.setCreateDate(dc.getCreateDate());
		if(dc.getCreateBy().equals(BussinessCommon.getUserId())) {
			editable = true;
		}
		
		this.attachments = dc.getAttachments();
	}
}
