package com.vz.backend.business.dto;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.DocumentReceive;
import org.springframework.data.domain.Sort.Direction;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentStatusEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class FindDocDto {
	private int no;
	private Long id;
	private String preview;
	private String numberOrSign;
	private Long numberArrival;
	private String numberArrivalStr;
	private Long urgentId;
	private Long securityId;
	private Long docFieldsId;
	private String docTypeName;
	private Long docTypeId;
	private String orgName;
	private DocumentStatusEnum docStatusId;
	private DocumentStatusEnum status;
	private String docStatusName;
	private Date currentDeadline;
	
	private Long orgIssuedId;
	private Date createFrom;
	private Date createTo;
	
	private Date dateIssuedFrom;
	private Date dateIssuedTo;
	
	private Long placeSendId;
	private String placeSend;
	private String signerName;
	private String placeReceive;
	private Long orgReceiveId;
	private Date dateArrivalFrom;
	private Date dateArrivalTo;
	private Date dateReceivedFrom;
	private Date dateReceivedTo;
	
	private String sortBy;
	private Direction direction;
	private int pageSize;
	
	private String urgentName;
	private String securityName;
	private String docFieldsName;
	private Long parentId;

	private Long bookId;

	private Boolean expired;

	private List<DocumentReceive> listReceive;

	public FindDocDto convert(FindDocDto dto) {
		dto.preview = BussinessCommon.convert(dto.preview);
		dto.numberOrSign = BussinessCommon.convert(dto.numberOrSign);
		dto.numberArrivalStr = BussinessCommon.convert(dto.numberArrivalStr);
		return dto;
	}

	public FindDocDto(Documents doc) {
		this.id = doc.getId();
		this.preview = doc.getPreview();
		this.numberArrival = doc.getNumberArrival();
		this.numberArrivalStr = doc.getNumberArrivalStr();
		this.docTypeName = "Văn bản đến";
		this.numberOrSign = doc.getNumberOrSign();
		if(doc.getListChildren() != null && doc.getListChildren().size() >0){
			for(Documents documents: doc.getListChildren()){
				if(documents.getOrgReceive() != null && documents.getStatus() != DocumentStatusEnum.RETAKE_ORG){
					if(this.orgName == null)this.orgName = documents.getOrgReceive().getName();
					else this.orgName = this.orgName+", "+documents.getOrgReceive().getName();
				}
				if(documents.getUserReceive() != null && documents.getStatus() != DocumentStatusEnum.RETAKE_ORG){
					if(this.orgName == null)this.orgName = documents.getUserReceive().getFullName();
					else this.orgName = this.orgName+", "+documents.getUserReceive().getFullName();
				}
			}
		} else {
			this.orgName = doc.getOrgReceive()!=null?doc.getOrgReceive().getName():doc.getUserReceive().getFullName();
		}
		
		this.urgentId = doc.getUrgentId();
		this.securityId = doc.getSecurityId();
		this.docFieldsId = doc.getDocFieldsId();
		this.status = doc.getStatus();
		this.docStatusName = this.status != null ? this.status.getName() : "";
		this.currentDeadline = doc.getDeadline();
		this.urgentName = doc.getUrgentName();
		this.securityName = doc.getSecurityName();
		this.docFieldsName = doc.getDocFieldsName();
		this.parentId =doc.getParentId();
		this.dateIssuedTo=doc.getDateIssued();
		this.dateArrivalTo= doc.getDateArrival();
		this.placeSend = doc.getPlaceSend();
		this.signerName=doc.getSignerName();
		this.orgIssuedId = doc.getOrgIssuedId();
		this.docTypeId = doc.getDocTypeId();
		this.expired = false ;
	}

	public FindDocDto(DocumentOut doc) {
		this.id = doc.getId();
		this.preview = doc.getPreview();
		this.numberOrSign = doc.getNumberOrSign();
		this.docTypeName = "Văn bản đi";
		this.orgName = doc.getOrg() == null ? "" : doc.getOrg().getName();
		this.urgentId = doc.getUrgentId();
		this.securityId = doc.getSecurityId();
		this.docFieldsId = doc.getDocFieldId();
		this.status = doc.getStatus();
		this.docStatusName = this.status != null ? this.status.getName() : "";
		this.dateIssuedTo=doc.getDateIssued();
		this.placeReceive= doc.getPlaceReceive();
		this.signerName=doc.getSignerName();
		this.listReceive=doc.getListReceive();
		this.orgIssuedId = doc.getOrgIssuedId();
		this.docTypeId = doc.getDocTypeId();
		this.createFrom = doc.getCreateDate();
	}
}
