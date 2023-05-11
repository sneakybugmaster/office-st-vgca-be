package com.vz.backend.business.dto.kpi;

import java.util.Date;

import com.vz.backend.business.domain.DocumentInProcess;
import com.vz.backend.business.domain.DocumentOutProcess;
import com.vz.backend.business.domain.TaskExecute;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentOutHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.HandleTypeEnum;

import lombok.Data;

@Data
public class KPIDataDto {
	private DocumentTypeEnum objType;
	private Long id;
	private Long objId;
	private Long userId;
	private Date incurredDate;
	private Long delegateId;
	private Date deadline;
	private Long personEnterId;
	private Long assingerId;
	private Integer step;
	private Boolean mergedLines;
	
	private Boolean taskHandleTypeMain;
	private Boolean taskHandleTypeSupport;
	private HandleTypeEnum docHandleType;
	
	private DocumentStatusEnum docStatus;
	private Integer taskStatus;
	
	private DocumentInHandleStatusEnum docInHandleStatus;
	private DocumentOutHandleStatusEnum docOutHandleStatus;
	private Integer taskHandleStatus;
	private Boolean endTask;
	
	public KPIDataDto(DocumentInProcess p) {
		this.objType = DocumentTypeEnum.VAN_BAN_DEN;
		this.id = p.getId();
		this.objId = p.getDocId();
		this.userId = p.getToUser();
		this.deadline = p.getDeadline();
		this.docInHandleStatus = p.getHandleStatus();
		this.docHandleType = p.getHandleType();
		this.docStatus = p.getDocument().getStatus();
		this.incurredDate = p.getUpdateDate();
		this.delegateId = p.getDelegaterId();
		this.mergedLines = p.getDocument().getMergedLines();
		this.endTask = p.getEndTask();
	}

	public KPIDataDto(DocumentOutProcess p) {
		this.objType = DocumentTypeEnum.VAN_BAN_DI;
		this.id = p.getId();
		this.objId = p.getDocId();
		this.userId = p.getUserId();
		this.docOutHandleStatus = p.getHandleStatus();
		this.docStatus = p.getDocumentOut().getStatus();
		this.incurredDate = p.getUpdateDate();
		this.delegateId = p.getDelegateId();
		this.personEnterId = p.getDocumentOut().getPersonEnterId();
	}
	
	public KPIDataDto(TaskExecute p) {
		this.objType = DocumentTypeEnum.GIAO_VIEC;
		this.id = p.getId();
		this.objId = p.getTaskId();
		this.userId = p.getUserId();
		this.taskHandleStatus = p.getStatus();
		this.taskStatus = p.getTask().getStatus();
		this.incurredDate = p.getUpdateDate();
		this.taskHandleTypeMain = p.getIsExcute();
		this.taskHandleTypeSupport = p.getIsCombination();
		this.deadline = p.getTask().getEndDate();
		this.assingerId = p.getTask().getUserAssignId();
		this.step = p.getStep();
	}

}
