package com.vz.backend.business.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.util.Constant;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SearchTask {
	private Long userCombinationId;
	private String taskName;
	private Long taskFieldId;
	private Long priorityId;
	private Boolean taskType;
	private Boolean taskStatus;
	private String codeTask;
	private Date startDate;
	private Date endDate;
	private Integer dayLeft;
	private Boolean configFields;
	private String userExcutePrimaryName;
	private Long orgId;
	private Boolean userStatus;
	@JsonIgnore
	private Long userAssignId;
	@JsonIgnore
	private Long userExcutePrimaryId;
	@JsonIgnore
	private List<Integer> status;
	@JsonIgnore
	private Long userId = BussinessCommon.getUserId();
	@JsonIgnore
	private Long clientId = BussinessCommon.getClientId();

	public void convert(Long userAssignId, Long userExcutePrimaryId) {
		this.taskName = BussinessCommon.convert(this.taskName);
		this.codeTask = BussinessCommon.convert(this.codeTask);
		this.userExcutePrimaryName = BussinessCommon.convert(this.userExcutePrimaryName);
		this.startDate = DateTimeUtils.getEndDate(DateTimeUtils.getYesterday(this.startDate));
		this.endDate = DateTimeUtils.getEndDate(this.endDate);
		this.userAssignId = userAssignId;
		this.userExcutePrimaryId = userExcutePrimaryId;
		this.status = this.getStatus(this.taskStatus);
		if (this.userStatus != null && (this.userExcutePrimaryName == null || "".equals(this.userExcutePrimaryName))) {
			this.userExcutePrimaryName = BussinessCommon.getUser().getFullName().toLowerCase();
		}
	}

	private List<Integer> getStatus(Boolean status) {
		List<Integer> sta = new ArrayList<>();
		if (status == null) {
			sta.add(Constant.TASK_STATUS_CLOSE);
			sta.add(Constant.TASK_STATUS_NEW);
			sta.add(Constant.TASK_STATUS_REJECT);
			sta.add(Constant.TASK_STATUS_INPROCESS);
			sta.add(Constant.TASK_STATUS_COMPLETE);
			sta.add(Constant.TASK_STATUS_REVOKE);
		} else {
			if (Boolean.TRUE.equals(status)) {
				sta.add(Constant.TASK_STATUS_CLOSE);
				sta.add(Constant.TASK_STATUS_REVOKE);
			} else {
				sta.add(Constant.TASK_STATUS_NEW);
				sta.add(Constant.TASK_STATUS_REJECT);
				sta.add(Constant.TASK_STATUS_INPROCESS);
				sta.add(Constant.TASK_STATUS_COMPLETE);
			}
		}
		return sta;
	}
}
