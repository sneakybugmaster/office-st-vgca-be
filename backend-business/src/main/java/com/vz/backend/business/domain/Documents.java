package com.vz.backend.business.domain;

import java.util.Date;
import java.util.List;

import javax.persistence.*;

import org.hibernate.annotations.NamedNativeQueries;
import org.hibernate.annotations.NamedNativeQuery;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.dto.outsideconnect.Content;
import com.vz.backend.business.util.Constant;
import com.vz.backend.core.auth.SecurityContext;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.CategoryEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.util.StringUtils;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@SqlResultSetMapping(name = "documentsMenuResult", classes = {
		@ConstructorResult(targetClass = com.vz.backend.business.dto.ReportDocByTypeDto.class, columns = {
				@ColumnResult(name = "count"),
				@ColumnResult(name = "status"),
				@ColumnResult(name = "mergedLines")
		}) 
})
@NamedNativeQueries(value = {
		@NamedNativeQuery(name = "Documents.menu", query = Constant.DOC_IN_MENU_QUERY, resultSetMapping = "documentsMenuResult"), })
@Table(name = "DOCUMENT", schema = "vz", indexes = {@Index(name = "DOC_INX_DOCUMENT",columnList = "id,book_id,number_arrival,place_send_id,doctype_id,number_sign,person_sign_id,status,node,org_transfer")})
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties(value = {"handler", "hibernateLazyInitializer"})
public class Documents extends DocumentBaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "book_id")
	private Long bookId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "book_id", insertable = false, updatable = false)
	private DocumentBook documentBook;

	@Column(name = "number_arrival")
	private Long numberArrival;

	@Column(name = "number_support")
	private String numberSupport;

	@Column(name = "place_send")
	private String placeSend;
	@Column(name = "place_send_id")
	private Long placeSendId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "place_send_id", insertable = false, updatable = false)
	private Category placeSends;

	@Column(name = "doctype_id")
	private Long docTypeId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doctype_id", insertable = false, updatable = false)
	private Category docType;

	private Boolean isImported = false;

	private Boolean isInternalDocument = false;

	@Column(name = "number_sign")
	private String numberOrSign;

	@Column(name = "date_arrival")
	@Temporal(TemporalType.TIMESTAMP)
	private Date dateArrival; //Ngày văn bản

	@Column(name = "date_issued")
	@Temporal(TemporalType.TIMESTAMP)
	private Date dateIssued;//Ngày vào sổ

	@Column(columnDefinition = "TEXT", name = "preview")
	private String preview;

	@Column(name = "person_sign_id")
	private Long personSignId;

	@Column(name = "person_sign")
	private String personSign;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "person_sign_id", insertable = false, updatable = false)
	private User userSign;

	@Column(name = "send_envelope")
	private boolean sendEnvelope;

	@Column(name = "urgent_id")
	private Long urgentId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "urgent_id", insertable = false, updatable = false)
	private Category urgent;

	@Column(name = "security_id")
	private Long securityId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "security_id", insertable = false, updatable = false)
	private Category security;

	@Column(name = "field_id")
	private Long docFieldsId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "field_id", insertable = false, updatable = false)
	private Category docFields;

	@Enumerated(EnumType.STRING)
	@Column(name = "status")
	private DocumentStatusEnum status;

	@Column(name = "day_left")
	private int dayLeft;

	@Column(name = "deadline")
	@Temporal(TemporalType.TIMESTAMP)
	private Date deadline;

	@Column(name = "org_issued_id")
	private Long orgIssuedId;

	@Column(name = "person_enter_id")
	private Long personEnterId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "person_enter_id", updatable = false, insertable = false)
	private User personEnter;

	@Column(name = "method_receipt_id")
	private Long methodReceiptId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "method_receipt_id", insertable = false, updatable = false)
	private Category methodReceipt;

	@Column(name = "rq_rely")
	private boolean rqRely;

	@Column(name = "is_legal")
	private boolean legalDoc;

	@Column(name = "feedback")
	private boolean feedback;

	@Column(name = "content_Process")
	private String contentProcess;

	@OneToMany(fetch = FetchType.EAGER, mappedBy = "documentId")
	private List<Attachment> attachments;

	@Column(name = "status_receipt_id")
	private Long statusReceiptId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "status_receipt_id", insertable = false, updatable = false)
	private Category statusReceipt;

	@Column(name = "org_issued_name")
	private String orgIssuedName;

	@Column(name = "node")
	private Long node;

	@Column(name = "date_received")
	@Temporal(TemporalType.TIMESTAMP)
	private Date receivedDate;//Ngày nhận văn bản

	@Column(name = "number_arrival_str")
	private String numberArrivalStr;

	@JsonIgnore
	@JoinColumn(name = "id_cat")
	private Long idCat;

	@Column(name = "parent_id")
	private Long parentId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "parent_id", insertable = false, updatable = false)
	private Documents parent;

	@JsonIgnore
	@OneToMany(fetch = FetchType.LAZY)
	@JoinColumn(name = "parent_id", insertable = false, updatable = false)
	private List<Documents> listChildren;
	
	@JsonIgnore
	@OneToMany(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_in_id", insertable = false, updatable = false)
	private List<DocumentInOut> listResponseDoc;

	@Column(name = "org_transfer")
	private Long orgTransferId;

	@ManyToOne
	@JoinColumn(name = "org_transfer", insertable = false, updatable = false)
	private Organization orgTransfer;

	@Column(name = "org_receive")
	private Long orgReceiveId;

	@ManyToOne
	@JoinColumn(name = "org_receive", insertable = false, updatable = false)
	private Organization orgReceive;

	@Column(name = "user_receive")
	private Long userReceiveId;

	@ManyToOne
	@JoinColumn(name = "user_receive", insertable = false, updatable = false)
	private User userReceive;
	
	@Column(name = "pre_node")
	private Long preNode;

	@Column(name = "type_org")
	private Long typeOrg;
	
	private Boolean global;

	@PreUpdate
	public void addDoc() {
		this.idCat = CategoryEnum.DOCUMENT.getValue();
	}

	@PrePersist
	public void setPersonDocAndCatId() {
		User user = SecurityContext.getCurrentUser();
		if (user != null) {
			this.personEnterId = user.getId();
		}
		this.idCat = CategoryEnum.DOCUMENT.getValue();
		this.preNode = this.node;
	}

	public String getNumberArrivalStr() {
		return this.numberArrivalStr;
	}
	
	public Long getNumberArrival() {
		return this.numberArrival == null ? 0 : this.numberArrival.longValue();
	}

	public String getDocStatusName() {
		return this.status != null ? this.status.getName() : "";
	}

	public String getSecurityName() {
		return this.security != null ? this.security.getName() : "";
	}

	public String getPersonSignName() {
		return this.userSign != null ? this.userSign.getFullName() : this.personSign;
	}

	@Transient
	private boolean canRetake;

	@Transient
	private String reason;

	@Transient
	private Boolean important;
	
	/**
	 *Văn bản Thông luồng
	 */
	@Column(name = "merged_lines")
	private Boolean mergedLines;


	/**
	 * Văn bản tuyệt mật
	 */
	private Boolean confidential;
	
	private Long docOutId;

	private Boolean autoCreateSeal = false;

	
	public Documents(Documents doc, Long node, Long parentId, DocumentStatusEnum status, Long orgReceiveId) {
		this.numberSupport = doc.getNumberSupport();
		this.placeSendId = doc.getPlaceSendId();
		this.docTypeId = doc.getDocTypeId();
		this.numberOrSign = doc.getNumberOrSign();
		this.dateArrival = doc.getDateArrival();
		this.preview = doc.getPreview();
		this.urgentId = doc.getUrgentId();
		this.dateIssued = doc.getDateIssued();
		this.receivedDate = doc.getReceivedDate();
		this.deadline = doc.getDeadline();
		this.securityId = doc.getSecurityId();
		this.docFieldsId = doc.getDocFieldsId();
		this.methodReceiptId = doc.getMethodReceiptId();
		this.orgTransferId = BussinessCommon.getUser().getOrg();
		this.node = node;
		this.parentId = parentId;
		this.status = status;
		this.orgReceiveId = orgReceiveId;
	}

	public Documents(Documents doc, Long node, Long parentId, DocumentStatusEnum status, Long orgReceiveId,Long typeOrg, Long userReceiveId) {
		this.numberSupport = doc.getNumberSupport();
		this.placeSendId = doc.getPlaceSendId();
		this.docTypeId = doc.getDocTypeId();
		this.numberOrSign = doc.getNumberOrSign();
		this.dateArrival = doc.getDateArrival();
		this.preview = doc.getPreview();
		this.urgentId = doc.getUrgentId();
		this.dateIssued = doc.getDateIssued();
		this.receivedDate = doc.getReceivedDate();
		this.deadline = doc.getDeadline();
		this.securityId = doc.getSecurityId();
		this.docFieldsId = doc.getDocFieldsId();
		this.methodReceiptId = doc.getMethodReceiptId();
		this.orgTransferId = BussinessCommon.getUser().getOrg();
		this.node = node;
		this.parentId = parentId;
		this.status = status;
		this.orgReceiveId = orgReceiveId;
		this.typeOrg = typeOrg;
		this.userReceiveId = userReceiveId;
	}
	
	public Documents(Documents doc, Long node, Long parentId, DocumentStatusEnum status, Long orgReceiveId,
			Boolean confidential) {
		this(doc, node, parentId, status, orgReceiveId);
		this.confidential = confidential;
	}
	
	public Documents(DocumentOut doc, Long orgReceiveId, DocumentStatusEnum status) {
		this.docTypeId = doc.getDocTypeId();
		this.numberOrSign = doc.getNumberOrSign();
		this.preview = doc.getPreview();
		this.urgentId = doc.getUrgentId();
		this.dateIssued = doc.getDateIssued();
		this.deadline = doc.getDeadline();
		this.securityId = doc.getSecurityId();
		this.docFieldsId = doc.getDocFieldId();
		this.orgTransferId = BussinessCommon.getUser().getOrg();
		this.status = status;
		this.orgReceiveId = orgReceiveId;
		this.bookId = doc.getBookId();
		this.receivedDate = new Date();
		this.dateArrival = new Date();
		this.numberArrival = doc.getNumberInBook(); //need confirm
		if (DocumentStatusEnum.DOING.equals(status)) {
			this.mergedLines = true;
		}

		if (doc.getNumberInBook() != null) {
			this.numberArrivalStr = doc.getNumberInBook().toString();
		}

		if (doc.getNumberOrSign() != null) {
			this.numberArrivalStr = this.numberArrivalStr == null ? doc.getNumberOrSign() : this.numberArrivalStr + doc.getNumberOrSign();
		}
	}

	public void valids(Boolean createArrivalNumber) {
		BussinessCommon.require("Trích yếu", this.preview);
		if (createArrivalNumber) {
			BussinessCommon.require("Sổ văn bản", this.bookId);
			BussinessCommon.require("Số đến", this.numberArrival);
		}
		BussinessCommon.require("Loại văn bản", this.docTypeId);
		if (StringUtils.isNullOrEmpty(this.placeSendOthers)) {
			BussinessCommon.require("Nơi gửi", this.placeSendId);
		}

		BussinessCommon.validLengthData(this.preview, "Trích yếu", 500);
	}
	
	public void set(Documents d) {
		this.bookId = convert(d.getBookId(), this.bookId);
		this.numberArrival = convert(d.getNumberArrival(), this.numberArrival);
		this.numberArrivalStr = convert(d.getNumberArrivalStr(), this.numberArrivalStr);
		
		// #2757 Chỉ chỉnh sửa 1 số trường liên quan tới sổ văn bản
		if(Boolean.TRUE.equals(d.getConfidential())) return;
		this.numberSupport = convert(d.getNumberSupport(), this.numberSupport);
		this.placeSendId = convert(d.getPlaceSendId(), this.placeSendId);
		this.numberOrSign = convert(d.getNumberOrSign(), this.numberOrSign);
		this.dateArrival = convert(d.getDateArrival(), this.dateArrival);
		this.dateIssued = convert(d.getDateIssued(), this.dateIssued);
		this.preview = convert(d.getPreview(), this.preview);
		this.urgentId = convert(d.getUrgentId(), this.urgentId);
		this.securityId = convert(d.getSecurityId(), this.securityId);
		this.docFieldsId = convert(d.getDocFieldsId(), this.docFieldsId);
		this.methodReceiptId = convert(d.getMethodReceiptId(), this.methodReceiptId);
		this.statusReceiptId = convert(d.getStatusReceiptId(), this.statusReceiptId);
		this.receivedDate = convert(d.getReceivedDate(), this.receivedDate);
		this.docTypeId = convert(d.getDocTypeId(), this.docTypeId);
	}

	private <T> T convert(T oVal, T nVal) {
		if (nVal != null && nVal.toString().length() > 0 && !nVal.equals(oVal)) {
			return nVal;
		}

		return oVal;
	}
	
	public boolean getMergedLines() {
		return this.mergedLines == null ? Boolean.FALSE : this.mergedLines; 
	}
	
	public void setMergedLines(Boolean mergedLines) {
		this.mergedLines = Boolean.FALSE.equals(mergedLines)  ? null : mergedLines;
	}
	
	/**
	 * Nơi nhận bên ngoài khác
	 */
	@Transient
	private String placeSendOthers;
	
	public String getUrgentName() {
		return this.urgent != null ? this.urgent.getName() : "";
	}
	
	public String getDocFieldsName() {
		return this.docFields != null ? this.docFields.getName() : "";
	}
	
	// #2757 Văn bản chuyển từ trên đơn vị cấp trên xuống đích danh của cấp dưới có kèm theo văn bản mật
	public void hideDataByConfidentialDoc() {
		if (Boolean.TRUE.equals(this.getConfidential())) {
			this.setPreview("*****");
			this.setAttachments(null);
			this.setDateArrival(null);
			this.setDateIssued(null);
			this.setDeadline(null);
//			this.setPlaceSendId(null);
//			this.setSecurityId(null);
//			this.setDocFieldsId(null);
//			this.setDocTypeId(null);
//			this.setStatusReceiptId(null);
			this.setNumberOrSign(null);
		}
	}
	
	/**
	 * Văn bản nhận từ bên ngoài
	 * @param doc
	 */
	public Documents(Content doc) {
		this.numberOrSign = doc.getNumberOrSign();
		this.preview = doc.getPreview();
		this.dateIssued = doc.getDateIssued();
		this.orgTransferId = BussinessCommon.getUser().getOrg();
		this.receivedDate = new Date();
		this.dateArrival = new Date();
		if (doc.getNumberOrSign() != null) {
			this.numberArrivalStr = this.numberArrivalStr == null ? doc.getNumberOrSign() : this.numberArrivalStr + doc.getNumberOrSign();
		}
		this.status = DocumentStatusEnum.WAIT_RECEIVE;
		this.global = true;
	}

	public void setDocBaseModel() {
		User currentUser = BussinessCommon.getUser();
		String currentOrgCode = currentUser.getOrgModel() != null ? currentUser.getOrgModel().getIdentifier() : "";
		String identifierDocument = currentOrgCode + "."
				+ (this.getDateIssued() != null ? (this.getDateIssued().getYear() + "") : "") + "."
				+ this.getNumberArrival();
		this.setIdentifier(currentOrgCode);
		this.setDocCode(identifierDocument);
	}

	public void setDocBaseModel(User currentUser) {
		String currentOrgCode = currentUser.getOrgModel() != null ? currentUser.getOrgModel().getIdentifier() : "";
		String identifierDocument = currentOrgCode + "."
				+ (this.getDateIssued() != null ? (this.getDateIssued().getYear() + "") : "") + "."
				+ this.getNumberArrival();
		this.setIdentifier(currentOrgCode);
		this.setDocCode(identifierDocument);
	}

	private Boolean isComplete; // Hoàn thành văn bản ngay sau khi thêm mới
}
