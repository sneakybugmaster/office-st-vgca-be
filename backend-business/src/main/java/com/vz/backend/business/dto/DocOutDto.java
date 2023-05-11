package com.vz.backend.business.dto;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class DocOutDto {
	private int no;
	private Long docOutId;
	// private Long nodeId;
	private Long numberInBook;
	private String numberOrSign;
	private String preview;
	private String docTypeName;
	private String personEnter;
	private Date createDate;
	private Date dateIssued;
	private Long urgentId;
	private Long securityId;
	// private DocumentStatusEnum status;
	// private List<DocumentOutAttachment> attachments;
}
