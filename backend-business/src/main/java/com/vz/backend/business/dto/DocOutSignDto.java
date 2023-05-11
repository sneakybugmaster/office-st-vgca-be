package com.vz.backend.business.dto;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.DocumentOutAttachment;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class DocOutSignDto {
	private long no;
	private Long docOutId;
	private Long processId;
	private Boolean important;
	private Long nodeId;
	private Long numberInBook;
	private String numberOrSign;
	private String preview;
	private String docTypeName;
	private String personEnter;
	private Date createDate;
	private String status;
	private String signerName;
	private String orgCreateName;
	private List<DocumentOutAttachment> attachments;
}
