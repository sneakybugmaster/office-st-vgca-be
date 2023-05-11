package com.vz.backend.business.domain;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import javax.persistence.*;

import org.hibernate.annotations.NamedNativeQueries;
import org.hibernate.annotations.NamedNativeQuery;
import org.hibernate.annotations.Where;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.dto.CategoryDto;
import com.vz.backend.business.util.Constant;
import com.vz.backend.core.auth.SecurityContext;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.CategoryEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;

@Entity
@SqlResultSetMapping(name = "documentOutMenuResult", classes = {
		@ConstructorResult(targetClass = com.vz.backend.business.dto.ReportDocByTypeDto.class, columns = {
				@ColumnResult(name = "count"),
				@ColumnResult(name = "status")
		}) 
})
@NamedNativeQueries(value = {
		@NamedNativeQuery(name = "DocumentOut.menu", query = Constant.DOC_OUT_MENU_QUERY, resultSetMapping = "documentOutMenuResult"), })
@Table(name = "DOCUMENT_OUT", schema = "vz", indexes = {@Index(name = "DOC_OUT_INX_",columnList = "id,book_id,doctype_id,number_in_book," +
		"urgent_id,security_id,field_id,status,person_enter_id,org_issued_id")})
@Data
@NoArgsConstructor
@AllArgsConstructor
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId" })
public class DocumentOut extends DocumentBaseModel {
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

	@Column(name = "doctype_id")
	private Long docTypeId;
	@ManyToOne(fetch = FetchType.LAZY)
	@JsonIgnore
	@JoinColumn(name = "doctype_id", updatable = false, insertable = false)
	private Category docType;

	public CategoryDto getDocType2() {
		return new CategoryDto(this.docType);
	}

	@Column(name = "number_in_book")
	private Long numberInBook;

	@Column(name = "number_sign")
	private String numberOrSign;

	public void setNumberOrSign(String numberOrSign) {
		this.numberOrSign = DocumentOut.normalizeSign(numberOrSign);
	}

	public static String normalizeSign(String sign) {
		if (sign == null) {
			return sign;
		}
		return sign.replaceAll("\\s+","");
	}

	@Column(name = "date_issued")
	@Temporal(TemporalType.TIMESTAMP)
	private Date dateIssued;

	@Column(name = "deadline")
	@Temporal(TemporalType.TIMESTAMP)
	private Date deadline;

	@Column(columnDefinition = "TEXT", name = "preview")
	private String preview;

	@Column(name = "sub_number")
	private Long subNumber;

	@Transient
	private List<DocumentReceive> listReceive;

	private String internalReceiversDescription;


	@Column(name = "urgent_id")
	private Long urgentId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "urgent_id", updatable = false, insertable = false)
	private Category urgent;

	@Column(name = "security_id")
	private Long securityId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "security_id", updatable = false, insertable = false)
	private Category security;

	@Column(name = "field_id")
	private Long docFieldId; // lĩnh vực
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "field_id", updatable = false, insertable = false)
	private Category docField;

	@Enumerated(EnumType.STRING)
	@Column(name = "status")
	private DocumentStatusEnum status;

	public String getStatusName() {
		if (status == null) {
			return null;
		}
		return status.getName();
	}

	@Column(name = "person_enter_id", nullable = false)
	private Long personEnterId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "person_enter_id", updatable = false, insertable = false)
	private User userEnter;

	public String getFullName() {
		if (this.userEnter == null) {
			return "No name";
		}
		return this.userEnter.getFullName();
	}

	@Column(name = "reply_doc") // Là văn bản trả lời.
	private Boolean replyDoc;

	@Column(name = "reply_task") // Đính kèm công việc
	private Boolean replyTask;

	@Column(name = "sign_ca")
	private Boolean signCA;

	@Column(name = "reply_doc_id")
	private String replyDocIds;

	@JsonIgnore
	@OneToMany(fetch = FetchType.EAGER)
	@JoinColumn(name = "doc_out_id", insertable = false, updatable = false)
	private List<DocumentInOut> listRelateDoc;

	@Column(name = "relate_task_id")
	private String relateTaskIds;

	@JsonIgnore
	@OneToMany(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_out_id", insertable = false, updatable = false)
	private List<DocumentOutTask> listDocumentOutTask;

	@Column(name = "list_signers_id") // Danh sách người ký
	private String listSignerIds;

	@Column(name = "directive_doc") // Là văn bản chỉ đạo
	private Boolean directiveDoc;

	@Column(name = "auto_issue") // Tự động ban hành
	private Boolean autoIssued;

	@Column(name = "legal_doc")
	private Boolean legalDoc;

	@Column(name = "issued_on_portal")
	private Boolean issuedOnPortal;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "docId")
	private List<DocumentOutAttachment> attachments;

	// Là văn bản nội bộ
	@Column(name = "is_internal_document")
	private Boolean isInternalDocument;
	// Chỉ thị của ban giám đốc
	@Column(name = "director_guidance")
	private String directorGuidance;
	// Giám đốc bút phê
	@Column(name = "director_signer_name")
	private String directorSignersName;
	// Ngày giám đốc bút phê
	@Column(name = "director_signed_date")
	private Date directorSignedDate;


	public List<DocumentOutAttachment> getAttachments() {
		if (this.attachments == null) {
			return Collections.emptyList();
		}
		List<DocumentOutAttachment> result = new ArrayList<>(this.attachments.size());
		for (DocumentOutAttachment attachment : this.attachments) {
			if (Boolean.TRUE.equals(attachment.getActive())) {
				result.add(attachment);
			}
		}
		return result;
	}

	@Column(name = "org_create_name")
	private String orgCreateName;

	@Column(name = "total_page")
	private Long totalPage;

	@Column(name = "node_id")
	private Long nodeId;
	//	@JsonIgnore
	//	@ManyToOne(fetch = FetchType.LAZY)
	//	@JoinColumn(name = "node_id", updatable = false, insertable = false)
	//	private NodeModel2 node;

	@Column(name = "org_issued_id")
	private Long orgIssuedId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "org_issued_id", updatable = false, insertable = false)
	private Organization org;

	@Column(name = "is_encrypt")
	private Boolean encrypt;

	public boolean shouldEncrypt() {
		List<DocumentOutAttachment> att = this.getAttachments();
		return Boolean.TRUE.equals(this.encrypt) && this.password == null && att != null && !att.isEmpty();
	}

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "docId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	@Where(clause = "active = 'TRUE'")
	private List<OutsideReceiveDocument> outsideReceives = new ArrayList<>();

	@JsonIgnore
	@ToString.Exclude
	@Column(name = "password")
	private String password;

	@JsonIgnore
	@JoinColumn(name = "id_cat")
	private Long idCat;

	@Column(name = "list_signers_name")
	private String listSignersName;

	@Column(name = "count_issued")
	private Integer countIssued; // Số lượng bản ban hành (VB đi)

	@Transient
	private String bookName;

	@Transient
	private String docFieldsName;

	@Transient
	private String docSecurityName;

	@Transient
	private String docTypeName;

	@Transient
	private String docUrgentName;




	@PrePersist
	public void prePersist() {
		User user = SecurityContext.getCurrentUser();
		if (user != null) {
			this.personEnterId = user.getId();
		}

		if (status == null) {
			this.status = DocumentStatusEnum.DU_THAO;
		}

		if (this.getReplyDoc() == null) {
			this.replyDoc = false;
		}

		if (this.getDirectiveDoc() == null) {
			this.directiveDoc = false;
		}

		if (this.listReceive == null) {
			this.listReceive = new ArrayList<>();
		}

		this.idCat = CategoryEnum.DOCUMENT_OUT.getValue();
	}

	@PreUpdate
	public void preUpdate() {
		if (this.personEnterId == null) {
			User user = SecurityContext.getCurrentUser();
			if (user != null) {
				this.personEnterId = user.getId();
			}
		}

		this.idCat = CategoryEnum.DOCUMENT_OUT.getValue();
	}

	public void addSignUser(Long userId) {
		if (this.listSignerIds == null || this.listSignerIds.equals("")) {
			this.listSignerIds = userId.toString();
		}
		if (this.listSignerIds.indexOf(userId.toString()) < 0) {
			this.listSignerIds += "," + userId.toString();
		}
	}
	
	/**
	 * Tổ chức soạn thảo
	 */
	private Long orgCreateId;
	
	
	@Transient
	private Boolean issued; // nếu thêm mới ban hành issued=true

	private Boolean hasIssued; // nếu thêm mới chưa ban hành hasIssued = false (Menu VB ban hành)

	public void setDocBaseModel() {
		User currentUser = BussinessCommon.getUser();
		Date date = new Date();
		String currentOrgCode = currentUser.getOrgModel() != null ? currentUser.getOrgModel().getIdentifier() : "";
		SimpleDateFormat df = new SimpleDateFormat("yyyy");
		String identifierDocument = currentOrgCode + "."
				+ (this.getDateIssued() != null ? (df.format(this.getDateIssued()) + "") : (df.format(date) + "")) + "."
				+ this.getNumberInBook();
		this.setIdentifier(currentOrgCode);
		this.setDocCode(identifierDocument);
		this.setSignerName(this.getListSignersName());
		this.setSendDate(this.getDateIssued());
	}
}
