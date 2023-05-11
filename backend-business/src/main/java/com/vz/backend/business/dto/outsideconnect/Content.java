package com.vz.backend.business.dto.outsideconnect;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutAttachment;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class Content {
	private Long objId;
	private String docTypeName;
	private String numberOrSign;
	private Date dateIssued;
	private String preview;
	private String urgentName;
	private String securityName;
	private String docFieldName;
	private Long totalPage;
	private List<Attachment> atms = new ArrayList<>();

	public Content(DocumentOut out, List<DocumentOutAttachment> outAtms) {
		this.objId = out.getId();
		this.docTypeName = out.getDocType() != null ? out.getDocType().getName() : "";
		this.urgentName = out.getUrgent() != null ? out.getUrgent().getName() : "";
		this.securityName = out.getSecurity() != null ? out.getSecurity().getName() : "";
		this.docFieldName = out.getDocField() != null ? out.getDocField().getName() : "";
		this.numberOrSign = out.getNumberOrSign();
		this.preview = out.getPreview();
		this.totalPage = out.getTotalPage();
		this.dateIssued = out.getDateIssued();
		outAtms.forEach( i -> this.atms.add(new Attachment(i)));
	}
}
