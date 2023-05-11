package com.vz.backend.business.dto;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.dto.UserDto;
import lombok.Data;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Data

//@NoArgsConstructor
//@AllArgsConstructor
public class DocumentProcessDto implements Serializable {

	private static final long serialVersionUID = 1L;

	private Long docId;
	private Boolean important;
	private Long processId;
	private String numberOrSign;
	private CategoryDto docType;
	private Long docFieldId;
	private String preview;
	private UserDto userEnter;
	private Date createDate;
	private UserDto handleUser;
	private Date handleDate;
	private CategoryDto security;
	private String status;
	private Long nodeId;
	private List<DocumentOutAttachment> attachments;
	private List<Long> signerIds;
	private UserDto delegateUser; // Người ủy quyền
	private UserDto delegatedUser; // Người được ủy quyền
	private DocumentStatusEnum docStatus;
	private Long numberInBook;
	private boolean read;

	public DocumentProcessDto(Boolean important, Long processId, Long handleId, String handleUserName,
							  String handleFullName, Date handleDate, DocumentOut documentOut,
							  DocumentOutHandleStatusEnum status, Boolean read) {
		this(important, processId, handleId, handleUserName, handleFullName,
				handleDate, documentOut, status, null, null, null,
				null, null, null, read);
	}

	public DocumentProcessDto(Boolean important, Long processId, Long handleId, String handleUserName,
							  String handleFullName, Date handleDate, DocumentOut documentOut,
							  DocumentOutHandleStatusEnum status, Long delegateUserId,
							  String delegateUserName, String delegateFullName, Long delegatedUserId, String delegatedUserName,
							  String delegatedFullName, Boolean read) {
		super();
		this.docId = documentOut.getId();
		this.important = important;
		this.processId = processId;
		this.numberOrSign = documentOut.getNumberOrSign();
		if (documentOut.getDocType() != null) {
			this.docType = new CategoryDto(documentOut.getDocType().getId(), documentOut.getDocType().getName(), documentOut.getDocType().getCategoryTypeId());
		}
		
		this.docFieldId = documentOut.getDocFieldId();
		this.preview = documentOut.getPreview();
		if (documentOut.getUserEnter() != null) {
			this.userEnter = new UserDto(documentOut.getUserEnter().getId(), documentOut.getUserEnter().getUserName(), documentOut.getUserEnter().getFullName());
		}

		this.createDate = documentOut.getCreateDate();
		this.handleUser = new UserDto(handleId, handleUserName, handleFullName);
		this.handleDate = handleDate;
		if (documentOut.getSecurityId() != null) {
			this.security = new CategoryDto(documentOut.getSecurityId(), documentOut.getSecurity().getName(), documentOut.getSecurity().getCategoryTypeId());
		}

		this.status = status.getName();
		this.nodeId = documentOut.getNodeId();
		this.delegateUser = new UserDto(delegateUserId, delegateUserName, delegateFullName);
		this.delegatedUser = new UserDto(delegatedUserId, delegatedUserName, delegatedFullName);
		this.docStatus = documentOut.getStatus();
		this.numberInBook = documentOut.getNumberInBook();
		this.signerIds = new ArrayList<>();
		if (documentOut.getListSignerIds() != null) {
			for (String tmp : documentOut.getListSignerIds().split(",")) {
				try {
					this.signerIds.add(Long.parseLong(tmp));
				} catch (NumberFormatException e) {
				}
			}
		}

		this.read = DocumentOutHandleStatusEnum.CHO_XU_LY.equals(status) && read != null ? read : false;
	}
}
