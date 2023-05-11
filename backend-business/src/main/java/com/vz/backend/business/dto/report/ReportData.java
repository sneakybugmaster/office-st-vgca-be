package com.vz.backend.business.dto.report;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.controller.DocumentOutProcessController;
import com.vz.backend.business.dto.kpi.KPIDataDto;
import com.vz.backend.business.service.DocumentService;
import com.vz.backend.business.util.Constant;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.HandleTypeEnum;

import lombok.Getter;
import lombok.Setter;

@Getter
public class ReportData {
	private long inMainNotYet;
	private long inMainDid;
	private long inMainDone;
	private long inSupportNotYet;
	private long inSupportDid;
	private long inSupportDone;
	private long inShowNotYet;
	private long inShowDid;
	private long inShowDone;
	@Setter
	private long inWaitComment;

	private long outSignSigned; //  request-signe - menu trinhky
	private long outSignIssued; // issued2 - menu trinh ky
	private long outProcessNotYet; //waiting-handle
	private long outProcessWaitComment; // waiting-comment
	private long outProcessDid; //handled - menu xyly
	private long outProcessDone; //issued - menu xu ly
	@Setter
	private long outShowDone; // nhận để biết
	private long outShowNotYet; // nhận để biết

	private long tAssignNotYet;
	private long tAssignDone;
	private long tMainNotYet;
	private long tMainDone;
	private long tSupportNotYet;
	private long tSupportDone;

	@JsonIgnore
	private boolean clerical;
	@JsonIgnore
	private boolean returnPreviousNode;

	public ReportData(boolean clerical, boolean returnPreviousNode) {
		this.clerical = clerical;
		this.returnPreviousNode = returnPreviousNode;
	}
	
	public void setOutShow(List<Long> outShowDone, List<Long> outShowNotYet) {
		this.outShowDone = outShowDone.size();
		this.outShowNotYet = outShowNotYet.size();
	}

	/**
	 * main type : 0, support type : 1, show type : 2, assigner : 3 
	 * did : 0, not yet : 1, done : 2, comment : 3, signed : 4, issued 5
	 * 
	 * @param data
	 * @param type
	 */
	public void set(List<KPIDataDto> data, DocumentTypeEnum type) {
		if (DocumentTypeEnum.VAN_BAN_DEN.equals(type)) {
			this.inMainNotYet = setCondition(0, 1, type).filter(data, type);
			this.inMainDid = setCondition(0, 0, type).filter(data, type);
			this.inMainDone = setCondition(0, 2, type).filter(data, type);
			this.inSupportNotYet = setCondition(1, 1, type).filter(data, type);
			this.inSupportDid = setCondition(1, 0, type).filter(data, type);
			this.inSupportDone = setCondition(1, 2, type).filter(data, type);
			this.inShowNotYet = setCondition(2, 1, type).filter(data, type);
			this.inShowDone = setCondition(2, 2, type).filter(data, type);
			this.inShowDid = setCondition(2, 0, type).filter(data, type);
		}

		if (DocumentTypeEnum.VAN_BAN_DI.equals(type)) {
			this.outProcessNotYet = setCondition(0, 1, type).filter(data, type);
			this.outProcessWaitComment = setCondition(0, 3, type).filter(data, type);
			this.outProcessDid = setCondition(0, 0, type).filter(data, type);
			this.outProcessDone = setCondition(0, 2, type).filter(data, type);
			this.outSignSigned = setCondition(0, 4, type).filter(data, type);
			this.outSignIssued = setCondition(0, 5, type).filter(data, type);
		}

		if (DocumentTypeEnum.GIAO_VIEC.equals(type)) {
			this.tAssignNotYet = setCondition(3, 1, type).filter(data, type);
			this.tAssignDone = setCondition(3, 2, type).filter(data, type);
			this.tMainNotYet = setCondition(0, 1, type).filter(data, type);
			this.tMainDone = setCondition(0, 2, type).filter(data, type);
			this.tSupportNotYet = setCondition(1, 1, type).filter(data, type);
			this.tSupportDone = setCondition(1, 2, type).filter(data, type);
		}
	}

	private ReportCondition setCondition(int type, int status, DocumentTypeEnum objType) {
		Long userId = BussinessCommon.getUserId();
		ReportCondition c = new ReportCondition();
		List<DocumentStatusEnum> docStatus = null;
		if (DocumentTypeEnum.VAN_BAN_DEN.equals(objType)) {
			docStatus = DocumentService.getObjStatusByType(type, status, clerical, returnPreviousNode, "docStatus");
			List<DocumentInHandleStatusEnum> handleStatusList = DocumentService.getObjStatusByType(type, status,
					clerical, returnPreviousNode, "handleStatus");
			Integer endTask = null;
			if (status == 2) { // Done tab
				endTask = 2;
			} else if (status == 0) { // Did tab
				endTask = 0;
			}
			c.setDocIn(endTask, docStatus, handleStatusList, getType(type), null, null);
		}

		if (DocumentTypeEnum.VAN_BAN_DI.equals(objType)) {
			String action = convertToAction(status);
			List<DocumentOutHandleStatusEnum> handleStatusList = null;
			DocumentOutHandleStatusEnum[] handleStatusTmp = DocumentOutProcessController.getStatus(action);
			DocumentStatusEnum[] docStatusTmp = DocumentOutProcessController.getDocStatus(action);
			if (handleStatusTmp != null) {
				handleStatusList = Arrays.asList(handleStatusTmp);
			}

			if (docStatusTmp != null) {
				docStatus = Arrays.asList(docStatusTmp);
			}
			c.setDocOut(docStatus, handleStatusList, userId, getPersonEnter(status));
			c.setOrCompare(type == 0 && status == 5);
		}

		if (DocumentTypeEnum.GIAO_VIEC.equals(objType)) {
			List<Integer> taskStatus = getTaskStatus(convertToTaskStatus(status));
			Boolean main = convertToTypeHandleStatus(type);
			Long assignerId = null;
			Long executorId = null;
			if (main == null) {
				assignerId = userId;
			} else {
				executorId = userId;
			}
			c.setTask(taskStatus, null, main, assignerId, executorId, null, null);
		}

		return c;
	}

//	 main type : 0 support type : 1 show type : 2 assigner : 3
	private Boolean convertToTypeHandleStatus(int type) {
		Boolean main = null;
		switch (type) {
		case 0:
			main = true;
			break;
		case 1:
			main = false;
			break;
		case 2:
		case 3:
		case 4:
		case 5:
			break;
		default:
			break;
		}
		return main;
	}

	// did : 0 not yet : 1 done : 2 comment : 3 signed : 4, issued 5
	private Boolean convertToTaskStatus(int status) {
		Boolean taskStatus = null;
		switch (status) {
		case 2:
			taskStatus = true;
			break;
		case 1:
			taskStatus = false;
			break;
		case 0:
		case 3:
		case 4:
		case 5:
			break;
		default:
			break;
		}
		return taskStatus;
	}

	private List<Integer> getTaskStatus(Boolean statusTask) {
		List<Integer> status = new ArrayList<>();
		if (statusTask == null) {
			status.add(Constant.TASK_STATUS_CLOSE);
			status.add(Constant.TASK_STATUS_NEW);
			status.add(Constant.TASK_STATUS_REJECT);
			status.add(Constant.TASK_STATUS_INPROCESS);
			status.add(Constant.TASK_STATUS_COMPLETE);
		} else if (Boolean.TRUE.equals(statusTask)) {
			status.add(Constant.TASK_STATUS_CLOSE);
			status.add(Constant.TASK_STATUS_REVOKE);
		} else {
			status.add(Constant.TASK_STATUS_NEW);
			status.add(Constant.TASK_STATUS_REJECT);
			status.add(Constant.TASK_STATUS_INPROCESS);
			status.add(Constant.TASK_STATUS_COMPLETE);
		}
		return status;
	}

	private Long getPersonEnter(int status) {
		Long personEnter = null;
		switch (status) {
		case 4:
		case 5:
			personEnter = BussinessCommon.getUserId();
			break;
		case 1:
		case 3:
		case 0:
		case 2:
			break;
		default:
			break;
		}
		return personEnter;
	}

	// * main type : 0 support type : 1 show type : 2
	// * did : 0 not yet : 1 done : 2 comment : 3 signed : 4, issued 5
	private String convertToAction(int status) {
		String action = "";
		switch (status) {
		case 1:
			action = "waiting-handle";
			break;
		case 3:
			action = "waiting-comment";
			break;
		case 0:
			action = "handled";
			break;
		case 2:
			action = "issued"; // menu xu ly == done
			break;
		case 4:
			action = "request-signe";
			break;
		case 5:
			action = "issued2"; // menu trinh ki
			break;
		default:
			break;
		}
		return action;
	}

	private HandleTypeEnum getType(int type) {
		HandleTypeEnum typeEnum = null;
		switch (type) {
		case 0:
			typeEnum = HandleTypeEnum.MAIN;
			break;
		case 1:
			typeEnum = HandleTypeEnum.SUPPORT;
			break;
		case 2:
			typeEnum = HandleTypeEnum.SHOW;
			break;
		default:
			break;
		}
		return typeEnum;
	}
}
