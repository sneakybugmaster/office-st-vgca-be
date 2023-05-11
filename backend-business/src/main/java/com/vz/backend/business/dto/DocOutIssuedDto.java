package com.vz.backend.business.dto;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentReceive;
import com.vz.backend.business.domain.OutsideReceiveDocument;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;
import java.util.List;
import java.util.Objects;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class DocOutIssuedDto extends DocOutSignDto {
	private Date dateIssued;
	private String securityName;
	private List<String> showToKnow;
	private String personHandle;
	private DocumentOutHandleStatusEnum statusHandleEnum;
	private DocumentStatusEnum docStatusEnum;
	private boolean read;
	private List<OutsideReceiveDocument> outsideReceives;
	private List<DocumentReceive> listReceive;
	private String internalReceiversDescription;
	private Boolean canDelete;
	private Boolean canRecall;
	private Integer countIssued;

	public DocOutIssuedDto(DocumentOut d) {
		this.setDocOutId(d.getId());
		this.setNodeId(d.getNodeId());
		this.setCreateDate(d.getCreateDate());
		this.setNumberOrSign(d.getNumberOrSign());
		this.setPersonEnter(d.getUserEnter().getFullName());
		this.setPreview(d.getPreview());
		this.setAttachments(d.getAttachments());
		this.setStatus(d.getStatus() != null ? d.getStatus().getName() : "");
		this.setDateIssued(d.getDateIssued());
		this.setDocTypeName((d.getDocType() != null && d.getDocType().getName() != null) ? d.getDocType().getName() : "");
		this.setSecurityName((d.getSecurity() != null && d.getSecurity().getName() != null) ? d.getSecurity().getName() : "");
		this.setSignerName(d.getSignerName());
		this.setOrgCreateName(d.getOrgCreateName());
		this.setDocStatusEnum(d.getStatus());
		this.setOutsideReceives(d.getOutsideReceives());
		this.setListReceive(d.getListReceive());
		this.setCountIssued(d.getCountIssued());
		this.internalReceiversDescription = d.getInternalReceiversDescription();
		this.canDelete = !d.getStatus().equals(DocumentStatusEnum.DA_BAN_HANH) && Objects.equals(d.getCreateBy(), BussinessCommon.getUserId());
		this.canRecall = d.getStatus().equals(DocumentStatusEnum.DA_BAN_HANH);
	}
}
