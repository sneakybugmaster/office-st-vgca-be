package com.vz.backend.business.dto;

import java.math.BigInteger;
import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.HandleTypeEnum;

import lombok.Data;

@Data
public class KnowableDto {
	private Long id;
	private List<DocumentOutAttachment> attachments;
	private Long numberInBook;
	private String numberOrSign;
	private CategoryDto docType2;
	private String fullName;
	private Date dateIssued;
	private String preview;
	private DocumentStatusEnum status;
	Boolean important;
	private Boolean read;
	private HandleTypeEnum handleType;
	private String handleTypeName;
	private Long fromUserId;
	private Boolean canForward;
	private Boolean canAddUser;

	public String getStatusName() {
		if (status == null) {
			return null;
		}
		return status.getName();
	}

	public KnowableDto(Long id, Long numberInBook, String numberOrSign, Long typeId, String typeName, String fullName,
			Date dateIssued, String preview, DocumentStatusEnum status) {
		super();
		this.id = id;
		this.numberInBook = numberInBook;
		this.numberOrSign = numberOrSign;
		this.docType2 = new CategoryDto(typeId, typeName);
		if (fullName == null) {
			fullName = "No name";
		}
		this.fullName = fullName;
		this.dateIssued = dateIssued;
		this.preview = preview;
		this.status = status;
	}
	
	public KnowableDto(Long id, Long numberInBook, String numberOrSign, Long typeId, String typeName, String fullName,
			Date dateIssued, String preview, DocumentStatusEnum status, Boolean important) {
		super();
		this.id = id;
		this.numberInBook = numberInBook;
		this.numberOrSign = numberOrSign;
		this.docType2 = new CategoryDto(typeId, typeName);
		if (fullName == null) {
			fullName = "No name";
		}
		this.fullName = fullName;
		this.dateIssued = dateIssued;
		this.preview = preview;
		this.status = status;
		this.important = important;
	}
	
	

	public KnowableDto(Long id, Long numberInBook, String numberOrSign, Long typeId, String typeName, String fullName,
			Date dateIssued, String preview, DocumentStatusEnum status, Boolean important, Boolean read,
			HandleTypeEnum handleType) {
		super();
		this.id = id;
		this.numberInBook = numberInBook;
		this.numberOrSign = numberOrSign;
		this.docType2 = new CategoryDto(typeId, typeName);
		if (fullName == null) {
			fullName = "No name";
		}
		this.fullName = fullName;
		this.dateIssued = dateIssued;
		this.preview = preview;
		this.status = status;
		this.important = important;
		this.read = read;
		this.handleType = handleType;
		this.handleTypeName = handleType == null ? null : getHandleTypeName(handleType );
	}
	
	private String getHandleTypeName(HandleTypeEnum handleType) {
		if (HandleTypeEnum.MAIN.equals(handleType)) {
			return "Xử lý chính";
		}
		if (HandleTypeEnum.SUPPORT.equals(handleType)) {
			return "Phối hợp";
		}
		if (HandleTypeEnum.SHOW.equals(handleType)) {
			return "Nhận để biết";
		}
		return "";
	}
	
	public KnowableDto(Object id) {
		this.id = ((BigInteger) id).longValue();
	}
}
