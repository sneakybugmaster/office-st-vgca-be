package com.vz.backend.business.dto;

import java.math.BigInteger;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
public class ReportDocByTypeDto {
	@JsonProperty("DOC_IN_MAIN")
	private long docInMain;
	
	@JsonProperty("DOC_IN_INTERNAL")
	private long docInInternal;

	@JsonProperty("DOC_IN_SUPPORT")
	private long docInSupport;

	@JsonProperty("DOC_IN_KNOW")
	private long docInKnow;

	@JsonProperty("DOC_IN_DIRECTION")
	private long docInDirection;

	@JsonProperty("DOC_IN_OPINION")
	private long docInOpinion;
	
	@JsonProperty("DOC_IN_WAIT_RECEIVE")
	private long docInWaitReceive;

	@JsonProperty("DRAFT_LIST")
	private long draftList;

	@JsonProperty("DRAFT_HANDLE")
	private long draftHandle;

	@JsonProperty("DRAFT_ISSUED")
	private long draftIssued;
	
	@JsonProperty("DOCUMENT_IN_LIST")
	private long draftKnow;

	@JsonProperty("TASK_MAIN")
	private long taskMain;

	@JsonProperty("TASK_SUPPORT")
	private long taskSupport;

	@JsonProperty("TASK_ASSIGN")
	private long taskAssign;

	@JsonProperty("DOC_INTERNAL_WAITING")
	private long internalWaiting;

	@JsonProperty("DOC_INTERNAL_DOING")
	private long internalDoing;

	@JsonProperty("DOC_INTERNAL_RETURN")
	private long internalReturn;

	@JsonProperty("DOC_INTERNAL_PUBLISH")
	private long internalPublish;

	@JsonProperty("DOC_INTERNAL_REGISTER")
	private long internalRegister;

	@JsonProperty("DOC_INTERNAL_APPROVE")
	private long internalApprove;

	@JsonProperty("DOC_INTERNAL_PENDING")
	private long internalPendding;

	@JsonProperty("WORD_EDITOR")
	private long wordEditor;

	@JsonProperty("DOC_IN_DELEGATE")
	private long docInDelegate;

	@JsonProperty("DOC_OUT_DELEGATE")
	private long docOutDelegate;

	@JsonProperty("DOC_INTERNAL_INCOMING")
	private long docInternalIncoming;

	@JsonProperty("DOC_INTERNAL_ISSUED_INCOMING")
	private long docInternalIssuedIncoming;

	@JsonIgnore
	private Long taskId;

	@JsonIgnore
	private Long userId;

	@JsonIgnore
	private Long assignerId;

	@JsonIgnore
	private Boolean isCombination;
	
	@JsonIgnore
	private Integer step;
	
	@JsonIgnore
	private Integer taskStatus;

	@JsonIgnore
	private String status;

	@JsonIgnore
	private long count;
	
	@JsonIgnore
	private Boolean mergedLines;

	public ReportDocByTypeDto(String status, Long count) {
		this.status = status;
		this.count = count;
	}
	
	public ReportDocByTypeDto(Object count, String status) {
		this.status = status;
		this.count = ((BigInteger) count).longValue();
	}

	public ReportDocByTypeDto(Object count, String status, Object mergedLines) {
		this(count, status);
		this.mergedLines = mergedLines == null ? null : ((Boolean) mergedLines).booleanValue();
	}

	public ReportDocByTypeDto count(List<ReportDocByTypeDto> dtos, boolean docInternalRemove) {
		if (dtos == null || dtos.isEmpty())
			return this;
		long main = 0;
		long tmp = 0;
		for (ReportDocByTypeDto i : dtos) {
			// for doc-in
			if ("MAIN".equals(i.getStatus()) && (!Boolean.TRUE.equals(i.getMergedLines()))) {
				main += i.getCount();
			}
			if ("SUPPORT".equals(i.getStatus()) && (!Boolean.TRUE.equals(i.getMergedLines()))) this.docInSupport = i.getCount();
			if ("SHOW".equals(i.getStatus()) && (!Boolean.TRUE.equals(i.getMergedLines()))) this.docInKnow = i.getCount();
			if ("DIRECTION".equals(i.getStatus()) && (!Boolean.TRUE.equals(i.getMergedLines()))) this.docInDirection = i.getCount();
			if ("CHO_CHO_Y_KIEN".equals(i.getStatus())) this.docInOpinion = i.getCount();
			if ("WAIT_RECEIVE".equals(i.getStatus())) this.docInWaitReceive = i.getCount();
			
			if(!docInternalRemove) {
				if ("MAIN".equals(i.getStatus()) && Boolean.TRUE.equals(i.getMergedLines())) tmp += i.getCount();
				if ("SUPPORT".equals(i.getStatus()) && Boolean.TRUE.equals(i.getMergedLines())) tmp += i.getCount();
				if ("SHOW".equals(i.getStatus()) && Boolean.TRUE.equals(i.getMergedLines())) tmp += i.getCount();
				
				this.docInInternal = tmp;
			}
			this.docInMain = main + this.docInDirection;
			//for doc-out
			if ("DU_THAO".equals(i.getStatus())) this.draftList = i.getCount();
			if ("DRAFT_HANDLE".equals(i.getStatus())) this.draftHandle = i.getCount();
			if ("DRAFT_ISSUED".equals(i.getStatus())) this.draftIssued = i.getCount();
			if ("DOCUMENT_IN_LIST".equals(i.getStatus())) this.draftKnow = i.getCount();
		}
		return this;
	}

	public void setInternalWaiting(Long count) {
		if (count == null)
			this.internalWaiting = 0L;
		else
			this.internalWaiting = count;
	}

	public void setInternalDoing(Long count) {
		this.internalDoing = count;
			
	}

	public void setInternalReturn(Long count) {
		this.internalReturn = count;
	}

	public void setInternalPublish(Long count) {
		this.internalPublish = count;
	}

	public ReportDocByTypeDto(Long taskId, Long userId, Long assignerId, Boolean isCombination, Integer step, Integer taskStatus) {
		this.taskId = taskId;
		this.userId = userId;
		this.assignerId = assignerId;
		this.isCombination = isCombination;
		this.step = step;
		this.taskStatus = taskStatus;
	}

	public ReportDocByTypeDto(List<ReportDocByTypeDto> dtos) {
		Set<Long> countByUserPrimary = new HashSet<>();
		Set<Long> countByUserSupport = new HashSet<>();
		Set<Long> countByUserAssign = new HashSet<>();
		Long currentUser = BussinessCommon.getUserId();
		for (ReportDocByTypeDto i : dtos) {
			if (currentUser.equals(i.getUserId()) && i.getStep() != null && i.getStep().intValue() > 1) {
				if (Boolean.TRUE.equals(i.getIsCombination())) { // phối hợp
					countByUserSupport.add(i.getTaskId());
				} else {
					countByUserPrimary.add(i.getTaskId()); // xử lý chính
				}
			}

			// việc đã giao
			if (currentUser.equals(i.getAssignerId()) 
					&& i.getStep() != null && i.getStep().intValue() == 1
					&& i.getTaskStatus() != null && i.getTaskStatus().intValue() < 4) {
				countByUserAssign.add(i.getTaskId());
			}

			if (DocumentInHandleStatusEnum.CHO_XU_LY.toString().equals(i.getStatus()))
				this.wordEditor = i.getCount();

			if ("DOC_IN_DELEGATE".equals(i.getStatus()))
				this.docInDelegate = i.getCount();
			if ("DOC_OUT_DELEGATE".equals(i.getStatus()))
				this.docOutDelegate = i.getCount();
		}

		this.taskAssign = (long) countByUserAssign.size();
		this.taskMain = (long) countByUserPrimary.size();
		this.taskSupport = (long) countByUserSupport.size();
	}
}
