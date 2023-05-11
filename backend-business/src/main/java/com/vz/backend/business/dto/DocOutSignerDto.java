package com.vz.backend.business.dto;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.core.common.BussinessCommon;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class DocOutSignerDto extends DocOutSignDto {
	private boolean hasSigner;
	private List<Long> signerIds;

	public DocOutSignerDto(DocumentOut d) {
		this.setDocOutId(d.getId());
		this.setNodeId(d.getNodeId());
		this.setCreateDate(d.getCreateDate());
		this.setNumberOrSign(d.getNumberOrSign());
		this.setPersonEnter(d.getUserEnter().getFullName());
		this.setPreview(d.getPreview());
		this.setAttachments(d.getAttachments());
		this.setStatus(d.getStatus().getName());
		this.setDocTypeName(d.getDocType().getName());
		this.setHasSigner(false);
		Long[] listIds = BussinessCommon.castStringToLongArray(d.getListSignerIds());
		List<Long> signerIds = new ArrayList<>();
		if (!BussinessCommon.isEmptyArr(listIds)) {
			signerIds = Arrays.asList(listIds);
			this.setHasSigner(true);
		}
		this.setSignerIds(signerIds);
	}
}
