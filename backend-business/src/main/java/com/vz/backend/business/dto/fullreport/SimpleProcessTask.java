package com.vz.backend.business.dto.fullreport;

import java.util.Date;

import lombok.Getter;

public class SimpleProcessTask extends SimpleProcess {
	public SimpleProcessTask(Long id, Date updateDate, Integer approveStatus, Date deadline) {
		super(id, id, updateDate);
		if (approveStatus == null) {
			approveStatus = -1;
		}
		this.approveStatus = approveStatus;
		this.deadline = deadline;
	}

	@Getter
	private int approveStatus;
	
	@Getter
	private Date deadline;
}
