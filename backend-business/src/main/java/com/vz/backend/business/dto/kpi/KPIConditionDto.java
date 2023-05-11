package com.vz.backend.business.dto.kpi;

import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.vz.backend.business.domain.KPIUser;
import com.vz.backend.business.domain.Targets;
import com.vz.backend.business.util.Constant;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.FrequencyEnum;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.DateTimeUtils;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class KPIConditionDto {
	//for statistical
	private List<Integer> months;
	private Long kpiAppId;
	
	//for calculation KPI
	private FrequencyEnum frequency;
	private Integer month;
	private Integer quarter;
	private Integer year;
	private Long userId;
	private Date startDate;
	private Date endDate;
	
	private HandleTypeEnum docHandleType;
	private Boolean taskHandleTypeMain; 
	
	private DocumentStatusEnum docStatus;
	private Integer taskStatus;
	
	private List<DocumentInHandleStatusEnum> docInHandleStatus;
	private List<DocumentOutHandleStatusEnum> docOutHandleStatus;
	private List<Integer> taskHandleStatus;
	
	private Integer afterDeadline;
	private Integer beforeDeadline;
	
	/*
	 * filter by type
	 */
	private DocumentTypeEnum objType;
	
	private List<DocumentInHandleStatusEnum> docInHandleDone = Arrays.asList(DocumentInHandleStatusEnum.DA_XU_LY);
	private List<DocumentOutHandleStatusEnum> docOutHandleDone = Arrays.asList(DocumentOutHandleStatusEnum.DA_XU_LY);
	private List<Integer> taskHandleDone = Arrays.asList(Constant.TASK_EXCUTE_STATUS_REJECT,
			Constant.TASK_EXCUTE_STATUS_COMPLETE, Constant.TASK_EXCUTE_STATUS_CLOSE);

	public int filter(List<KPIDataDto> datas, DocumentTypeEnum type, Date frDateFilter, Date toDateFilter) {
		Set<Long> docInIds = new HashSet<>();
		Set<Long> docOutIds = new HashSet<>();
		Set<Long> taskIds = new HashSet<>();
		for (KPIDataDto i : datas) {
			if (!this.userId.equals(i.getUserId()) && !this.userId.equals(i.getDelegateId())) continue;
			
			if (DocumentTypeEnum.VAN_BAN_DEN.equals(i.getObjType())
					&& (this.docInHandleStatus == null || this.docInHandleStatus.contains(i.getDocInHandleStatus()))
					&& (this.docHandleType == null || this.docHandleType.equals(i.getDocHandleType()))
					&& (this.docStatus == null || this.docStatus.equals(i.getDocStatus()))
					&& (this.afterDeadline == null || i.getDeadline() == null || i.getIncurredDate().compareTo(i.getDeadline()) >= this.afterDeadline)
					&& (this.beforeDeadline == null || i.getDeadline() == null || i.getIncurredDate().compareTo(i.getDeadline()) <= this.beforeDeadline)
					&& (frDateFilter == null || i.getIncurredDate().compareTo(frDateFilter) >= 0)
					&& (toDateFilter == null || i.getIncurredDate().compareTo(toDateFilter) < 0)
					) {
				docInIds.add(i.getId());
			}
			
			if (DocumentTypeEnum.VAN_BAN_DI.equals(i.getObjType())
					&& (this.docOutHandleStatus == null || this.docOutHandleStatus.contains(i.getDocOutHandleStatus()))
					&& (this.docStatus == null || this.docStatus.equals(i.getDocStatus()))
					&& (frDateFilter == null || i.getIncurredDate().compareTo(frDateFilter) >= 0)
					&& (toDateFilter == null || i.getIncurredDate().compareTo(toDateFilter) < 0)
				) {
				docOutIds.add(i.getId());
			}
			
			if (DocumentTypeEnum.GIAO_VIEC.equals(i.getObjType())
					&& (this.taskHandleStatus == null || this.taskStatus.equals(i.getTaskHandleStatus()))
					&& (this.taskHandleTypeMain == null || this.taskHandleTypeMain.equals(i.getTaskHandleTypeMain()))
					&& (this.taskStatus == null || this.taskStatus.equals(i.getTaskStatus()))
					&& (this.afterDeadline == null || i.getDeadline() == null || i.getIncurredDate().compareTo(i.getDeadline()) >= this.afterDeadline)
					&& (this.beforeDeadline == null || i.getDeadline() == null || i.getIncurredDate().compareTo(i.getDeadline()) <= this.beforeDeadline)
					&& (frDateFilter == null || i.getIncurredDate().compareTo(frDateFilter) >= 0)
					&& (toDateFilter == null || i.getIncurredDate().compareTo(toDateFilter) < 0)
					) {
				taskIds.add(i.getId());
			}
		}
		
		switch (type) {
		case VAN_BAN_DEN:
			return docInIds.size();
		case VAN_BAN_DI:
			return docOutIds.size();
		case GIAO_VIEC:
			return taskIds.size();
		default:
			return 0;
		}
	}

	private Date getDate(FrequencyEnum frequency, Integer quarter, Integer month, Integer year, int option) {
		switch (frequency) {
		case MONTH:
			if (option == 1) {
				month = month == null ? 1 : month;
				return DateTimeUtils.firstDateOfMonth(month, year);
			}
			
			if (option == 2) {
				month = month == null ? 12 : month;
				return DateTimeUtils.lastDateOfMonth(month, year);
			}
			break;
		case QUARTER:
			if (option == 1)
				return DateTimeUtils.firstDayOfQuarter(quarter, year);
			if (option == 2)
				return DateTimeUtils.lastDateOfQuater(quarter, year);
			break;
		case YEAR:
			if (option == 1)
				return DateTimeUtils.firstDateOfYear(year);
			if (option == 2)
				return DateTimeUtils.lastDateOfYear(year);
			break;
		case SIX_MONTHS_FIRST_YEAR:
			if (option == 1)
				return DateTimeUtils.firstDateOfMonth(1, year);
			if (option == 2)
				return DateTimeUtils.lastDateOfMonth(6, year);
			break;
		case SIX_MONTHS_LAST_YEAR:
			if (option == 1)
				return DateTimeUtils.firstDateOfMonth(7, year);
			if (option == 2)
				return DateTimeUtils.lastDateOfMonth(12, year);
			break;
		default:
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
		}
		return new Date();
	}
	
	public KPIConditionDto setDateInfo() {
		this.validTime();
		this.startDate = getDate(this.frequency, this.quarter, this.month, this.year, 1);
		this.endDate = getDate(this.frequency, this.quarter, this.month, this.year, 2);
		return this;
	}
	
	public void setDocIn(DocumentStatusEnum docStatus, List<DocumentInHandleStatusEnum> status, HandleTypeEnum type, Long userId, Integer afterDeadline, Integer beforeDeadline) {
		this.docInHandleStatus = status;
		this.docHandleType = type;
		this.userId = userId;
		this.docStatus = docStatus;
		this.afterDeadline = afterDeadline;
		this.beforeDeadline = beforeDeadline;
		this.objType = DocumentTypeEnum.VAN_BAN_DEN;
	}
	
	public void setDocOut(DocumentStatusEnum docStatus, List<DocumentOutHandleStatusEnum> status, Long userId) {
		this.docOutHandleStatus = status;
		this.userId = userId;
		this.docStatus = docStatus;
		this.objType = DocumentTypeEnum.VAN_BAN_DI;
	}
	
	public void setTask(Integer taskStatus, List<Integer> status, Boolean taskHandleTypeMain, Long userId, Integer afterDeadline, Integer beforeDeadline) {
		this.taskHandleStatus = status;
		this.userId = userId;
		this.taskStatus = taskStatus;
		this.taskHandleTypeMain = taskHandleTypeMain;
		this.afterDeadline = afterDeadline;
		this.beforeDeadline = beforeDeadline;
		this.objType = DocumentTypeEnum.GIAO_VIEC;
	}
	
	public KPIConditionDto setCondition(KPIUser kpiUser) {
		Targets targets = kpiUser.getTargets();
		Long userId = kpiUser.getUserId();

		switch (targets.getSource()) {
		case DI_TOTAL_DOCUMENT_IN_DONE_WITH_ALL_ROLE:
			this.setDocIn(DocumentStatusEnum.DONE, docInHandleDone, null, userId, null, null);
			break;
		case DI_TOTAL_DOCUMENT_IN_DONE_WITH_MAIN_ROLE:
			this.setDocIn(DocumentStatusEnum.DONE, docInHandleDone, HandleTypeEnum.MAIN, userId, null, null);
			break;
		case DI_TOTAL_DOCUMENT_IN_DONE_WITH_SUPPORT_ROLE:
			this.setDocIn(DocumentStatusEnum.DONE, docInHandleDone, HandleTypeEnum.SUPPORT, userId, null, null);
			break;
		case DI_TOTAL_DOCUMENT_IN_DONE_WITH_SHOW_ROLE:
			this.setDocIn(DocumentStatusEnum.DONE, docInHandleDone, HandleTypeEnum.SHOW, userId, null, null);
			break;
		case DI_TOTAL_DOCUMENT_IN_DID_WITH_ALL_ROLE:
			this.setDocIn(null, docInHandleDone, null, userId, null, null);
			break;
		case DI_TOTAL_DOCUMENT_IN_DID_WITH_MAIN_ROLE:
			this.setDocIn(null, docInHandleDone, HandleTypeEnum.MAIN, userId, null, null);
			break;
		case DI_TOTAL_DOCUMENT_IN_DID_WITH_SUPPORT_ROLE:
			this.setDocIn(null, docInHandleDone, HandleTypeEnum.SUPPORT, userId, null, null);
			break;
		case DI_TOTAL_DOCUMENT_IN_DID_WITH_SHOW_ROLE:
			this.setDocIn(null, docInHandleDone, HandleTypeEnum.SHOW, userId, null, null);
			break;
		case DI_TOTAL_DOCUMENT_IN_DONE_AFTER_DEADLINE:
			this.setDocIn(DocumentStatusEnum.DONE, docInHandleDone, null, userId, 0, null);
			break;
		case DI_TOTAL_DOCUMENT_IN_DID_AFTER_DEADLINE:
			this.setDocIn(null, docInHandleDone, null, userId, 0, null);
			break;
		case DI_TOTAL_DOCUMENT_IN_DONE_BEFORE_DEADLINE:
			this.setDocIn(DocumentStatusEnum.DONE, docInHandleDone, null, userId, null, 0);
			break;
		case DI_TOTAL_DOCUMENT_IN_DID_BEFORE_DEADLINE:
			this.setDocIn(null, docInHandleDone, null, userId, null, 0);
			break;
		case DO_TOTAL_DOCUMENT_OUT_DONE:
			this.setDocOut(DocumentStatusEnum.DONE, docOutHandleDone, userId);
			break;
		case DO_TOTAL_DOCUMENT_OUT_DID:
			this.setDocOut(null, docOutHandleDone, userId);
			break;
		case T_TOTAL_TASK_DONE_WITH_ALL_ROLE:
			this.setTask(Constant.TASK_STATUS_CLOSE, taskHandleDone, null, userId, null, null);
			break;
		case T_TOTAL_TASK_DONE_WITH_MAIN_ROLE:
			this.setTask(Constant.TASK_STATUS_CLOSE, taskHandleDone, true, userId, null, null);
			break;
		case T_TOTAL_TASK_DONE_WITH_SUPPORT_ROLE:
			this.setTask(Constant.TASK_STATUS_CLOSE, taskHandleDone, false, userId, null, null);
			break;
		case T_TOTAL_TASK_DID_WITH_ALL_ROLE:
			this.setTask(null, taskHandleDone, null, userId, null, null);
			break;
		case T_TOTAL_TASK_DID_WITH_MAIN_ROLE:
			this.setTask(null, taskHandleDone, true, userId, null, null);
			break;
		case T_TOTAL_TASK_DID_WITH_SUPPORT_ROLE:
			this.setTask(null, taskHandleDone, false, userId, null, null);
			break;
		case T_TOTAL_TASK_DONE_AFTER_DEADLINE:
			this.setTask(Constant.TASK_STATUS_CLOSE, taskHandleDone, false, userId, 0, null);
			break;
		case T_TOTAL_TASK_DID_AFTER_DEADLINE:
			this.setTask(null, taskHandleDone, null, userId, 0, null);
			break;
		case T_TOTAL_TASK_DONE_BEFORE_DEADLINE:
			this.setTask(Constant.TASK_STATUS_CLOSE, taskHandleDone, false, userId, null, 0);
			break;
		case T_TOTAL_TASK_DID_BEFORE_DEADLINE:
			this.setTask(null, taskHandleDone, false, userId, null, 0);
			break;
		default:
			break;
		}
		return this;
	}

	private void validTime() {
		switch (this.frequency) {
		case MONTH:
//			if (this.month == null || this.year == null)
			if (this.year == null)
				throw new RestExceptionHandler(Message.NO_INPUT_DATA);
			break;
		case QUARTER:
			if (this.quarter == null || this.year == null)
				throw new RestExceptionHandler(Message.NO_INPUT_DATA);
			break;
		case YEAR:
		case SIX_MONTHS_FIRST_YEAR:
		case SIX_MONTHS_LAST_YEAR:
			if (this.year == null)
				throw new RestExceptionHandler(Message.NO_INPUT_DATA);
			break;
		default:
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
		}
	}
	
	public KPIConditionDto(FrequencyEnum frequency, Integer month, Integer year, Long userId) {
		this.frequency = frequency;
		this.month = month;
		this.year = year;
		this.userId = userId;
		this.setDateInfo();
	}
	
	public void validStatistical() {
		if (this.getKpiAppId() == null || this.getUserId() == null)
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
	}
}
