package com.vz.backend.business.dto;

import com.vz.backend.business.domain.DocumentOut;
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
public class DocumentBasicDto {
	Long id;
	String preview;
	String orgReceive;
	DocumentStatusEnum status;
	String statusName;
	Long typeOrg;

	public DocumentBasicDto(Documents doc) {
		this.id = doc.getId();
		this.preview = doc.getPreview();
		if (doc.getOrgReceive() != null) {
			this.orgReceive = doc.getOrgReceive().getName();
		}
		if(doc.getUserReceive() != null){
			this.orgReceive = doc.getUserReceive().getFullName();
		}
		this.status = doc.getStatus();
		if (doc.getStatus() != null) {
			this.statusName = doc.getStatus().getName();
		}
		this.typeOrg = doc.getTypeOrg() != null ? doc.getTypeOrg() : 1L;
	}
	
	public DocumentBasicDto(DocumentOut doc) {
		this.id = doc.getId();
		this.preview = doc.getPreview();
		this.status = doc.getStatus();
		if (doc.getStatus() != null) {
			this.statusName = doc.getStatus().getName();
		}
	}
}
