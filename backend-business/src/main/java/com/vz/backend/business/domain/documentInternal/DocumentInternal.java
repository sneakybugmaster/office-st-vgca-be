package com.vz.backend.business.domain.documentInternal;

import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.Table;

import com.vz.backend.business.domain.DocumentBook;
import com.vz.backend.business.dto.document.DocInternalCreateDto;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "DOCUMENT_INTERNAL", schema = "vz")
@Getter
@Setter
@NoArgsConstructor
public class DocumentInternal extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "number_sign") //Số văn bản
	private String numberOrSign;

	@Column(columnDefinition = "TEXT", name = "preview") //Trích yếu
	private String preview;
	
	@Column(name = "org_create_id") //Đơn vị soạn thảo
	private Long orgCreateId;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "org_create_id", updatable = false, insertable = false)
	private Organization org;
	
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "create_by", updatable = false, insertable = false)
	private User createUser;

	@Column(name = "signer_id") //Lãnh đạo ký
	private Long signerId;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "status")
	private DocumentStatusEnum status;
	
	@Column(name = "approve_date")
	private Date approveDate;
	
	@Column(name = "[read]")
	private Boolean read = false;

	@OneToMany(fetch = FetchType.LAZY, mappedBy = "docId") //File văn bản và file phụ lục.
	private List<DocInternalAttach> listAttachment;
	
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "docId") //Người duyệt
	private List<DocInternalApprove> listApprove;
	
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "docId") //Danh sách phòng ban (Thực hiện/Xem)
	private List<DocInternalReceiver> listReceiver;
	
//	@OneToMany(fetch = FetchType.LAZY, mappedBy = "docId")
//	private List<DocInternalTracking> listTracking;

//	public List<DocInternalAttachment> getAttachments() {
//		if (this.listAttachment == null) {
//			return null;
//		}
//		List<DocInternalAttachment> result = new ArrayList<>(this.listAttachment.size());
//		for (DocInternalAttachment attachment : this.listAttachment) {
//			if (Boolean.TRUE.equals(attachment.getActive())) {
//				result.add(attachment);
//			}
//		}
//		return result;
//	}

	@Column(name = "security_id")
	private Long securityId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "security_id", updatable = false, insertable = false)
	private Category security;

	@Column(name = "urgent_id")
	private Long urgentId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "urgent_id", updatable = false, insertable = false)
	private Category urgent;

	@Column(name = "doc_type_id")
	private Long docTypeId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_type_id", updatable = false, insertable = false)
	private Category docType;

	@Column(name = "doc_book_id")
	private Long docBookId;

	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_book_id", updatable = false, insertable = false)
	private DocumentBook docBook;

	@Column(name = "doc_date")
	private Date docDate;

	@Column(name = "sign_date")
	private Date signDate;

	@Column(name = "issued_quantity")
	private Integer issuedQuantity;

	@PrePersist
	public void prePersist() {
	}

	@PreUpdate
	public void preUpdate() {
	}

	public DocumentInternal(DocInternalCreateDto doc) {
		super();
		this.numberOrSign = doc.getNumberOrSign();
		this.preview = doc.getPreview();
		this.signerId = doc.getSignerId();
		this.status = doc.getStatus();
		this.orgCreateId = doc.getOrgCreateId();
		this.securityId = doc.getSecurityId();
		this.urgentId = doc.getUrgentId();
		this.docTypeId = doc.getDocTypeId();
		this.signDate = doc.getSignDate();
		this.docDate = doc.getDocDate();
		this.docBookId = doc.getDocBookId();
		this.issuedQuantity = doc.getIssuedQuantity();
	}
}
