package com.vz.backend.business.domain.standard;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.MappedSuperclass;
import javax.persistence.OneToOne;
import javax.persistence.Temporal;
import javax.persistence.TemporalType;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.domain.DocumentBook;
import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.domain.hstl.HsFolderFile;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.Category;

import lombok.Data;
import lombok.NoArgsConstructor;
@MappedSuperclass
@Data
@NoArgsConstructor
public class BaseDoc extends BaseCode {
	
	@Column(name = "book_id")
	private Long bookId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "book_id", insertable = false, updatable = false)
	private DocumentBook documentBook;
	
	/**
	 * Mã định danh - độ dài 25
	 * 000.00.00.G09.2010.01.TH là Mã của hồ sơ số 01 nhóm Tổng hợp, năm 2010, Bộ Nội vụ
	 */
	@Column(name = "doc_code")
	private String docCode;
	
	/**
	 * Mã hồ sơ --> hệ thống tự gen 
	 */
	@Column(name = "file_code")
	private String fileCode;
	
	/**
	 * FileCatalog
	 * Mục lục số hoặc năm hình thành hồ sơ - 4
	 */
	@Column(name = "file_catalog")
	private Integer fileCatalog;
	
	/**
	 * FileNotation
	 * Số và ký hiệu hồ sơ - độ dài 20
	 */
	@Column(name = "file_notation")
	private String fileNotation;
	
	/**
	 * Số thứ tự văn bản trong hồ sơ
	 */
	@Column(name = "doc_ordinal")
	private Integer docOrdinal;
	
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
	
	@Column(name = "doctype_id")
	private Long docTypeId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doctype_id", insertable = false, updatable = false)
	private Category docType;
	
	/**
	 * Số của văn bản - độ dài 11
	 */
	@Column(name = "code_number")
	private Long codeNumber;
	
	/**
	 * Ký hiệu của văn bản - độ dài 30
	 */
	@Column(name = "code_notation")
	private String codeNotation;
	
	/**
	 * Ngày ban hành - DD/MM/YYYY = Ngày văn bản
	 */
	@Column(name = "issued_date")
	@Temporal(TemporalType.TIMESTAMP)
	private Date issuedDate;
	
	/**
	 * Trích yếu nội dung - độ dài 500
	 */
	@Column(columnDefinition = "TEXT", name = "subject")
	private String subject;
	
	/**
	 * ngôn ngữ -- độ dài 100
	 */
	@Column(name = "language")
	private String language;
	
	/**
	 * Số lượng trang của văn bản - độ dài 4
	 */
	@Column(name = "page_amount")
	private Integer pageAmount;
	
	/**
	 * ghi chú - 500
	 */
	@Column(name = "description", columnDefinition = "TEXT")
	private String description;
	
	/**
	 * Ký hiệu thông tin - 30
	 */
	@Column(name = "infor_sign")
	private String inforSign;
	
	/**
	 * từ khóa - 100
	 */
	@Column(name = "keyword")
	private String keyword;
	
	/**
	 * Chế độ sử dụng - 20
	 */
	@Column(name = "mode")
	private String mode;
	
	/**
	 * Mức độ tin cậy - 30
	 */
	@Column(name = "confidence_level")
	private String confidenceLevel;
	
	/**
	 * Bút tích - 2000
	 */
	@Column(name = "autograph", columnDefinition = "TEXT")
	private String autograph;
	
	
	/**
	 * Tình trạng vật lý - 50
	 */
	@Column(name = "format")
	private String format;

	@Override
	public void valids() {
		BussinessCommon.require("Mã định danh văn bản", this.docCode);
		BussinessCommon.validLengthData(this.docCode, "Mã định danh văn bản", 25);
		BussinessCommon.validLengthData(this.subject, "Trích yếu nội dung", 500);
		BussinessCommon.require("Mục lục số hoặc năm hình thành hồ sơ", this.fileCatalog);
		BussinessCommon.require("Số và ký hiệu hồ sơ", this.fileNotation);
		BussinessCommon.validLengthData(this.autograph, "Bút tích", 2000);
		BussinessCommon.validLengthData(this.description, "Ghi chú", 500);
	}
	
	public BaseDoc(Documents doc) {
//		this.docCode = doc.getDocCode();
//		this.fileCode = doc.getFileCode();
//		this.fileCatalog = doc.getFileCatalog();
		this.fileNotation = doc.getNumberOrSign();
		super.setIdentifier(BussinessCommon.getUser().getOrgModel().getIdentifier()); 
//		super.setOrganld(doc.getOrganld());
//		this.docOrdinal = doc.getDocOrdinal();
//		this.language = doc.getLanguage();
//		this.pageAmount = doc.getPageAmount();
//		this.description = doc.getDescription();
//		this.inforSign = doc.getInforSign();
//		this.keyword = doc.getKeyword();
//		this.mode = doc.getMode();
//		this.confidenceLevel = doc.getConfidenceLevel();
//		this.autograph = doc.getAutograph();
//		this.format = doc.getFormat();
		this.codeNumber = doc.getNumberArrival();
		this.codeNotation = doc.getNumberOrSign();
		this.subject = doc.getPreview();
		this.issuedDate = doc.getDateArrival();
	}
	
	public BaseDoc(DocumentOut doc) {
//		this.docCode = doc.getDocCode();
//		this.fileCode = doc.getFileCode();
//		this.fileCatalog = doc.getFileCatalog();
		this.fileNotation = doc.getNumberOrSign();
		super.setIdentifier(BussinessCommon.getUser().getOrgModel().getIdentifier()); 
//		super.setOrganld(doc.getOrganld());
//		this.docOrdinal = doc.getDocOrdinal();
//		this.language = doc.getLanguage();
//		this.pageAmount = doc.getPageAmount();
//		this.description = doc.getDescription();
//		this.inforSign = doc.getInforSign();
//		this.keyword = doc.getKeyword();
//		this.mode = doc.getMode();
//		this.confidenceLevel = doc.getConfidenceLevel();
//		this.autograph = doc.getAutograph();
//		this.format = doc.getFormat();
		this.codeNumber = doc.getNumberInBook();
		this.codeNotation = doc.getNumberOrSign();
		this.subject = doc.getPreview();
		this.issuedDate = doc.getDateIssued();
	}
	
	public void set(HsFolderFile f) {
		this.bookId = f.getBookId();
		this.docCode = f.getDocCode();
		this.fileCode = f.getFileCode();
		this.fileCatalog = f.getFileCatalog();
		this.fileNotation = f.getFileNotation();
		this.docOrdinal = f.getDocOrdinal();
		this.urgentId = f.getUrgentId();
		this.securityId = f.getSecurityId();
		this.docTypeId = f.getDocTypeId();
		this.codeNumber = f.getCodeNumber();
		this.codeNotation = f.getCodeNotation();
		this.issuedDate = f.getIssuedDate();
		this.subject = f.getSubject();
		this.language = f.getLanguage();
		this.pageAmount = f.getPageAmount();
		this.description = f.getDescription();
		this.inforSign = f.getInforSign();
		this.keyword = f.getKeyword();
		this.mode = f.getMode();
		this.confidenceLevel = f.getConfidenceLevel();
		this.autograph = f.getAutograph();
		this.format = f.getFormat();
		this.issuedDate = f.getIssuedDate();
	}
}
