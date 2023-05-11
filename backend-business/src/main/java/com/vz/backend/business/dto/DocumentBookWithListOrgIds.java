package com.vz.backend.business.dto;

import java.util.List;

import com.vz.backend.business.domain.DocumentBook;

import lombok.Data;

@Data
public class DocumentBookWithListOrgIds {
	private DocumentBook db;
	private List<Long> orgIds;
	private List<Long> categoryIds;

	public void setId(Long id) {
		db.setId(id);
	}
}
