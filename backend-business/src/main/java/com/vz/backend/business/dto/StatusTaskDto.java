package com.vz.backend.business.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class StatusTaskDto {
	private int finish;
	private int Processing;
	private int waitingForReception;
	private int overDue;
	private int returnTask;
}
