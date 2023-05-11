package com.vz.backend.business.dto.hstl;

import java.util.HashSet;
import java.util.Set;

import com.vz.backend.business.dto.fullreport.ReportKey;
import com.vz.backend.core.config.DocumentTypeEnum;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public class DocumentByMonth extends ReportKey {

	Set<Long> denIdSet = new HashSet<>();
	Set<Long> diIdSet = new HashSet<>();


	public void add(Long docId, DocumentTypeEnum docType) {
		if (docId != null) {
			switch (docType) {
			case VAN_BAN_DEN:
				denIdSet.add(docId);
				break;
			case VAN_BAN_DI:
				denIdSet.add(docId);
				break;
			default:
				break;
			}
		}
	}

	public int getDenSize() {
		return denIdSet.size();
	}

	public int getDiSize() {
		return diIdSet.size();
	}

	public DocumentByMonth(ReportKey key) {
		this.setKey(key.getKey());
		this.setYear(key.getYear());
		this.setPart(key.getPart());
	}
}
