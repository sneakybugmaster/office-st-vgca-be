package com.vz.backend.business.dto.hstl.export;

import java.util.List;

import com.vz.backend.business.domain.hstl.HsFolderDocument;
import com.vz.backend.business.domain.hstl.HsFolderFile;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Getter
public class ContentDoc {
	/**
	 * Số thứ tự
	 */
	@Setter
	private int no;

	/**
	 * Số kí hiệu văn bản
	 */
	private String codeNumberNotation;

	/**
	 * Ngày văn bản
	 */
	private String issuedDate;

	/**
	 * Tên loại và trích yếu nội dung văn bản
	 */
	private String subject;

	/**
	 * Tác giả văn bản
	 */
	private String creator;

	/**
	 * Tờ số/Trang số Số thứ tự văn bản trong hồ sơ
	 */
	private String docOrdinal;

	/**
	 * ghi chú
	 */
	private String description;

	private List<ContentDoc> fileList;
	private List<ContentDoc> docList;

	public <T> ContentDoc(T tmp) {
		if (tmp instanceof HsFolderDocument) {
			HsFolderDocument f = (HsFolderDocument) tmp;
			this.codeNumberNotation = BussinessCommon.toString(f.getCodeNumber()) + BussinessCommon.toString(f.getCodeNotation());
			this.creator = f.getCreateByObj() != null ? BussinessCommon.toString(f.getCreateByObj().getFullName()) : "";
			this.description = BussinessCommon.toString(f.getDescription());
			this.issuedDate = DateTimeUtils.formatDate(f.getIssuedDate(), "dd/MM/yyyy");
			this.docOrdinal = BussinessCommon.toString(f.getDocOrdinal());
			this.subject = BussinessCommon.toString(f.getSubject());
		} else if (tmp instanceof HsFolderFile) {
			HsFolderFile f = (HsFolderFile) tmp;
			this.codeNumberNotation = BussinessCommon.toString(f.getCodeNumber()) + BussinessCommon.toString(f.getCodeNotation());
			this.creator = f.getCreateByObj() != null ? BussinessCommon.toString(f.getCreateByObj().getFullName()) : "";
			this.description = BussinessCommon.toString(f.getDescription());
			this.issuedDate = DateTimeUtils.formatDate(f.getIssuedDate(), "dd/MM/yyyy");
			this.docOrdinal = BussinessCommon.toString(f.getDocOrdinal());
			this.subject = BussinessCommon.toString(f.getSubject());
		}
	}

	public static void setNumber(List<ContentDoc> folders, int start) {
		int no = 1;
		for (int i = start; i < folders.size() + start; i++) {
			folders.get(i).setNo(no + start);
			no++;
		}
	}
}
