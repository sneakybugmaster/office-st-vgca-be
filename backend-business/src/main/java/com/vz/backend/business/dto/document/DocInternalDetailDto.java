package com.vz.backend.business.dto.document;

import java.util.*;

import com.vz.backend.business.domain.documentInternal.DocumentInternal;
import com.vz.backend.core.config.DocumentStatusEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class DocInternalDetailDto {
	private Long docId;
	private String numberOrSign;
	private String preview;
	private Long createBy;
	private Long orgCreateId;
	private Long docBookId;
	private Long docTypeId;
	private Long urgentId;
	private Long securityId;
	private String userCreateName;
	private String orgCreateName;
	private String docTypeName;
	private String docBookName;
	private String securityName;
	private String urgentName;
	private Date createDate;
	private Date signDate;
	private Date docDate;
	private DocumentStatusEnum docStatus;
	private List<ApproverDto> listSigner; // Danh sách người ký
	private List<ApproverDto> listApprover; // Danh sách thông tin phê duyệt
	private List<ApproverDto> listReturn; // Danh sách thông tin trả lại
	private List<DocInternalReceiverDto> listReceiver; // Danh sách nơi nhận
	private List<DocInternalAttachDto> listAttachment; // Danh sách đính kèm
	private boolean canRetake;
	private Integer issuedQuantity;


	public DocInternalDetailDto(DocumentInternal doc) {
		super();
		this.docId = doc.getId();
		this.docBookId = doc.getDocBookId();
		this.docTypeId = doc.getDocTypeId();
		this.securityId = doc.getSecurityId();
		this.urgentId = doc.getUrgentId();
		this.orgCreateId = doc.getOrgCreateId();
		this.issuedQuantity = doc.getIssuedQuantity();
		this.numberOrSign = doc.getNumberOrSign();
		this.preview = doc.getPreview();
		this.createBy = doc.getCreateBy();
		this.userCreateName = doc.getCreateUser().getFullName();
		this.orgCreateName = doc.getOrg().getName();
		this.createDate = doc.getCreateDate();
		this.signDate = doc.getSignDate();
		this.docDate = doc.getDocDate();
		this.docStatus = doc.getStatus();
		this.docTypeName = doc.getDocType().getName();
		this.docBookName = doc.getDocBook().getName();
		if (doc.getSecurity() != null) {
			this.securityName = doc.getSecurity().getName();
		}
		if (doc.getUrgent() != null) {
			this.urgentName = doc.getUrgent().getName();
		}
	}
}
