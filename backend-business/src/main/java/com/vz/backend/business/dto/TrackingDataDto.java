package com.vz.backend.business.dto;

import java.util.Date;

import lombok.Getter;

@Getter
public class TrackingDataDto {
	private Long pId;
	private Long frUser;
	private Long toUser;
	private String action;
	private String handleStatus;
	private Date deadine;
	private String comment;
	private Integer progress;
	private int step;
	private Long node;
	private String delegateInfo;
	private String frInfo;
	private String toInfo;
	
	public TrackingDataDto(TrackingDto dto) {
		super();
		this.pId = dto.getPId();
		this.frUser = dto.getFrUser();
		this.toUser = dto.getToUser();
		this.action = dto.getAction();
		this.handleStatus = dto.getHandleStatus();
		this.deadine = dto.getDeadine();
		this.comment = dto.getComment();
		this.progress = dto.getProgress();
		this.step = dto.getStep();
		this.node = dto.getNode();
		this.delegateInfo = dto.getDelegateInfo();
		this.frInfo = dto.getFrInfo();
		this.toInfo = dto.getToInfo();
	}
}
