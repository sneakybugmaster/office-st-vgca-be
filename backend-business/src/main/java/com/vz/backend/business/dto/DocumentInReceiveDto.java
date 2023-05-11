package com.vz.backend.business.dto;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.Attachment;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.core.config.DocumentStatusEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@Data
@NoArgsConstructor
@AllArgsConstructor
public class DocumentInReceiveDto {
	private Long id;
	private String preview;
	private String numberOrSign;
	private String orgTransfer;
	private Date deadline;
	private Date dateArrival;
	private Date receivedDate;
	private DocumentStatusEnum status;
	private List<Attachment> attachments;
	private Boolean important;
	private boolean read;
	private Boolean confidential;

	public DocumentInReceiveDto(Documents doc) {
		// #2757 Văn bản chuyển từ trên đơn vị cấp trên xuống đích danh của cấp dưới có kèm theo văn bản mật
		doc.hideDataByConfidentialDoc();
		this.id = doc.getId();
		this.preview = doc.getPreview();
		this.numberOrSign = doc.getNumberOrSign();
		if (doc.getOrgTransfer() != null) {
			this.orgTransfer = doc.getOrgTransfer().getName();
		} else {
			this.orgTransfer = "";
		}
		this.deadline = doc.getDeadline();
		this.dateArrival = doc.getDateArrival();
		this.receivedDate = doc.getReceivedDate();
		this.status = doc.getStatus();
		this.attachments = doc.getAttachments();
		this.confidential = doc.getConfidential();
	}

	public DocumentInReceiveDto(Documents doc, Boolean important, Boolean read) {
		this(doc);
		this.important = important;
		this.read = read == null ? false : read;
	}

	public String getDocStatusName() {
		return this.status != null ? this.status.getName() : "";
	}
}
