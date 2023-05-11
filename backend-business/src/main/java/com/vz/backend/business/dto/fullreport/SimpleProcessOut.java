package com.vz.backend.business.dto.fullreport;

import java.util.Date;

import com.vz.backend.core.config.DocumentOutHandleStatusEnum;

import lombok.Getter;

public class SimpleProcessOut extends SimpleProcess {
	public SimpleProcessOut(Long id, Long docId, Date updateDate, DocumentOutHandleStatusEnum handleStatus) {
		super(id, docId, updateDate);
		this.handleStatus = handleStatus;
	}

	@Getter
	private DocumentOutHandleStatusEnum handleStatus;

}
