package com.vz.backend.business.dto;

import java.util.Date;

import com.vz.backend.business.domain.WordEditorProcess;
import com.vz.backend.core.common.BussinessCommon;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class TrackingReceiveDocDto {
	private Integer progress;
	private String comment;
	private String positionRe;
	private String action;
	private String statusHandle;
	private String fullNameRe;
	private String org;
	private Date deadline;
	private int no;
	//private List<TaskAttachment> attachments;
	
	public TrackingReceiveDocDto(WordEditorProcess j, int no) {
		super();
		this.positionRe = j.getToUser().getPositionModel().getName();
		this.action = BussinessCommon.getAction(j.getHandleStatus(), true);
		this.fullNameRe = j.getToUser().getFullName();
		this.org = j.getToUser().getOrgModel().getName();
		this.no = no;
	}
}
