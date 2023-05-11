package com.vz.backend.business.dto.fullreport;

import java.util.Date;

import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.HandleTypeEnum;

import lombok.Getter;

public class SimpleProcessIn extends SimpleProcess {
	public SimpleProcessIn(Long id, Long docId, Date updateDate, DocumentInHandleStatusEnum handleStatus, Date deadline) {
		super(id, docId, updateDate);
		this.handleStatus = handleStatus;
		this.deadline = deadline;
	}
	
	public SimpleProcessIn(Long id, Long docId, Date updateDate, HandleTypeEnum handleType, DocumentInHandleStatusEnum handleStatus, Date deadline) {
		this(id, docId, updateDate, handleStatus, deadline);
		this.handleType = handleType;
	}

	@Getter
	private DocumentInHandleStatusEnum handleStatus;
	
	@Getter
	private HandleTypeEnum handleType;
	
	@Getter
	private Date deadline;

}
