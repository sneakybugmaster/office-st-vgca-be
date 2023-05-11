package com.vz.backend.business.dto;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.AttachmentVersion;
import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.domain.DocumentReceive;
import com.vz.backend.business.domain.OutsideReceiveDocument;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.dto.SignerDto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class DocumentOutDetailDto {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	private String personEnterName;
	private String bookName;
	private String docFieldsName;
	private String docTypeName;
	private String docStatusName;
	private String docUrgentName;
	private String docSecurityName;
	private List<DocumentReplyDto> listReplyDoc;
	private List<SignerDto> listSigners;
	private List<DocumentReceive> listReceive;
	private List<AttachmentVersion> listAttachVersion;
	private List<TaskDto> listRelateTask; //Công việc liên quan.
	private List<TaskDto> listReplyTask; //Công việc trả lời.
	
	private List<DocumentOutAttachment> attachments; // Tạo dto.
	//private List<CommentDto> listComments; // Tạo dto.
	
	private Long id;
	private String numberOrSign;
	private String preview;
	private Date dateIssued;
	private Long subNumber;
	private DocumentStatusEnum status;
	private Boolean replyDoc;
	private Boolean directiveDoc;
	private Boolean autoIssued;
	private Boolean legalDoc;
	private String orgCreateName;
	private Long totalPage;
	private Long nodeId;
	private Boolean issuedOnPortal;
	private Date deadline;
	private Boolean encrypt;
	private Boolean signCA;
	private Boolean replyTask;
	private Long numberInBook;
	private Long docTypeId;
	private Long docFieldId;
	private Long urgentId;
	private Long securityId;
	private Long bookId;
	private String replyDocIds;
	private String relateTaskIds;
	private String listSignerIds;
	private boolean editable;
	private List<OutsideReceiveDocument> outsideReceives;
	private String listSignersName;
	private Boolean canForward;
	private Boolean canAddUser;
	private String documentDetail;
	private String placeReceive;
	private String docCode;
	private String identifier;
	private Date sendDate;
	private Date receiveDate ;
	private String signerName ;
	private Integer countIssued;
	private String internalReceiversDescription;
	private Long incomingDocumentId;

	
	/**
	 * Tổ chức soạn thảo
	 */
	private Long orgCreateId;
	
	public DocumentOutDetailDto(DocumentOut documentOut) {
		this.setId(documentOut.getId());
		//this.setClientId(documentOut.getClientId());
		this.setCountIssued(documentOut.getCountIssued());
		this.setNumberInBook(documentOut.getNumberInBook());
		if (documentOut.getNumberOrSign() != null) {
			this.setNumberOrSign(documentOut.getNumberOrSign());
		} else if (documentOut.getNumberInBook() != null) {
			this.setNumberOrSign(documentOut.getNumberInBook().toString());
		}
		this.setDateIssued(documentOut.getDateIssued());
		this.setPreview(documentOut.getPreview());
		this.setSubNumber(documentOut.getSubNumber());
		this.setListReceive(documentOut.getListReceive());
		this.setStatus(documentOut.getStatus());
		//this.setPersonEnterId(documentOut.getPersonEnterId());
		//this.setUserEnter(documentOut.getUserEnter());
		this.setReplyDoc(documentOut.getReplyDoc());
		this.setReplyDocIds(documentOut.getReplyDocIds());
		this.setDirectiveDoc(documentOut.getDirectiveDoc());
		this.setAutoIssued(documentOut.getAutoIssued());
		this.setLegalDoc(documentOut.getLegalDoc());
		this.setAttachments(documentOut.getAttachments());
		this.setOrgCreateName(documentOut.getOrgCreateName());
		this.setTotalPage(documentOut.getTotalPage());
		this.setDocTypeId(documentOut.getDocTypeId());
		this.setDocFieldId(documentOut.getDocFieldId());
		this.setUrgentId(documentOut.getUrgentId());
		this.setSecurityId(documentOut.getSecurityId());
		this.setNodeId(documentOut.getNodeId());
		this.setListSignerIds(documentOut.getListSignerIds());
		this.setBookId(documentOut.getBookId());
		this.setIssuedOnPortal(documentOut.getIssuedOnPortal());
		this.setDeadline(documentOut.getDeadline());
		this.setEncrypt(documentOut.getEncrypt());
		this.setSignCA(documentOut.getSignCA());
		this.setReplyTask(documentOut.getReplyTask());
		this.setRelateTaskIds(documentOut.getRelateTaskIds());
		this.setOutsideReceives(documentOut.getOutsideReceives());
		this.listSignersName = documentOut.getListSignersName();
		this.orgCreateId = documentOut.getOrgCreateId();
		this.documentDetail = documentOut.getDocumentDetail();
		this.placeReceive = documentOut.getPlaceReceive();
		this.docCode = documentOut.getDocCode();
		this.identifier = documentOut.getIdentifier();
		this.sendDate = documentOut.getSendDate();
		this.receiveDate = documentOut.getReceiveDate();
		this.signerName = documentOut.getSignerName();
		this.internalReceiversDescription = documentOut.getInternalReceiversDescription();
	}
}
