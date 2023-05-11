package com.vz.backend.business.dto.report;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.vz.backend.business.dto.kpi.KPIDataDto;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.HandleTypeEnum;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class ReportCondition {
	private HandleTypeEnum docHandleType;
	private Boolean isMain;
	private Long assignerId;
	
	private List<DocumentStatusEnum> docStatus;
	private List<Integer> taskStatus;
	
	private List<DocumentInHandleStatusEnum> docInHandleStatus;
	private List<DocumentOutHandleStatusEnum> docOutHandleStatus;
	private List<Integer> taskHandleStatus;
	
	private Integer afterDeadline;
	private Integer beforeDeadline;
	private Long personEnterId;
	private Long userId;
	private Boolean mergedLines;
	private Boolean orCompare;
	
	/*
	 * filter by type
	 */
	private DocumentTypeEnum objType;
	private Integer endTask;
	
	public long filter(List<KPIDataDto> data, DocumentTypeEnum type) {
		Set<Long> docInIds = new HashSet<>();
		Set<Long> docOutIds = new HashSet<>();
		Set<Long> taskIds = new HashSet<>();
		for (KPIDataDto i : data) {
			List<DocumentStatusEnum> doingStatus = Arrays.asList(DocumentStatusEnum.DOING, DocumentStatusEnum.RETURN_DOC);
			if (DocumentTypeEnum.VAN_BAN_DEN.equals(i.getObjType())
					&& (this.docInHandleStatus == null || this.docInHandleStatus.contains(i.getDocInHandleStatus()))
					&& (this.docHandleType == null || this.docHandleType.equals(i.getDocHandleType()))
					&& (this.docStatus == null || this.docStatus.contains(i.getDocStatus()))
					&& (this.afterDeadline == null || i.getDeadline() == null || i.getIncurredDate().compareTo(i.getDeadline()) > this.afterDeadline)
					&& (this.beforeDeadline == null || i.getDeadline() == null || i.getIncurredDate().compareTo(i.getDeadline()) <= this.beforeDeadline)
					&& (!Boolean.TRUE.equals(i.getMergedLines()))
					&& (this.endTask == null || (
						(doingStatus.contains(i.getDocStatus()) && Boolean.TRUE.equals(i.getEndTask()) && this.endTask.intValue() == 2)
						||	(DocumentStatusEnum.DONE.equals(i.getDocStatus()) && this.endTask.intValue() == 2)
						||	(doingStatus.contains(i.getDocStatus()) && i.getEndTask() == null && this.endTask.intValue() == 0))
						)
				) {
				docInIds.add(i.getObjId());
			}
			
			if (DocumentTypeEnum.VAN_BAN_DI.equals(i.getObjType())
					&& (this.docOutHandleStatus == null || this.docOutHandleStatus.contains(i.getDocOutHandleStatus()))
					&& (this.docStatus == null || this.docStatus.contains(i.getDocStatus()))
					&& ((orCompare && (this.userId == null || this.userId.equals(i.getUserId())
							|| this.personEnterId == null || this.personEnterId.equals(i.getPersonEnterId())))
							|| (!orCompare && (this.userId == null || this.userId.equals(i.getUserId()))
									&& (this.personEnterId == null || this.personEnterId.equals(i.getPersonEnterId()))))) {
				docOutIds.add(i.getObjId());
			}
			
			if (DocumentTypeEnum.GIAO_VIEC.equals(i.getObjType())
					&& (this.taskHandleStatus == null  || this.taskHandleStatus.contains(i.getTaskHandleStatus()) )
					&& ((this.assignerId == null && i.getStep() > 1) || (this.assignerId != null && this.assignerId.equals(i.getAssingerId()) && i.getStep() == 1))
					&& (this.userId == null || this.userId.equals(i.getUserId()))
					&& (this.isMain == null 
					|| (this.isMain.equals(i.getTaskHandleTypeMain()) && Boolean.TRUE.equals(this.isMain) && Boolean.FALSE.equals(i.getTaskHandleTypeSupport()))
					|| (this.isMain.equals(i.getTaskHandleTypeMain()) && Boolean.FALSE.equals(this.isMain) && Boolean.TRUE.equals(i.getTaskHandleTypeSupport())))
					&& (this.taskStatus == null || this.taskStatus.contains(i.getTaskStatus()))
					&& (this.afterDeadline == null || i.getDeadline() == null || i.getIncurredDate().compareTo(i.getDeadline()) >= this.afterDeadline)
					&& (this.beforeDeadline == null || i.getDeadline() == null || i.getIncurredDate().compareTo(i.getDeadline()) <= this.beforeDeadline)
				) {
				taskIds.add(i.getObjId());
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
	
	public ReportCondition setDocIn(Integer endTask, List<DocumentStatusEnum> docStatus, List<DocumentInHandleStatusEnum> status, HandleTypeEnum type, Integer afterDeadline, Integer beforeDeadline) {
		this.docInHandleStatus = status;
		this.docHandleType = type;
		this.docStatus = docStatus;
		this.afterDeadline = afterDeadline;
		this.beforeDeadline = beforeDeadline;
		this.objType = DocumentTypeEnum.VAN_BAN_DEN;
		this.endTask = endTask;
		return this;
	}
	
	public void setDocOut(List<DocumentStatusEnum> docStatus, List<DocumentOutHandleStatusEnum> status, Long userId, Long personEnterId) {
		this.docOutHandleStatus = status;
		this.docStatus = docStatus;
		this.objType = DocumentTypeEnum.VAN_BAN_DI;
		this.userId = userId;
		this.personEnterId = personEnterId;
	}
	
	public void setTask(List<Integer> taskStatus, List<Integer> handleStatus, Boolean isMain, Long assignerId,
			Long userId, Integer afterDeadline, Integer beforeDeadline) {
		this.taskHandleStatus = handleStatus;
		this.taskStatus = taskStatus;
		this.isMain = isMain;
		this.afterDeadline = afterDeadline;
		this.beforeDeadline = beforeDeadline;
		this.objType = DocumentTypeEnum.GIAO_VIEC;
		this.assignerId = assignerId;
		this.userId = userId;
	}
}
