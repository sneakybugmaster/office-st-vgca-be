package com.vz.backend.business.dto;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.domain.DocumentInProcess;
import com.vz.backend.business.service.DocumentInProcessService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.domain.User;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class TrackingDto {
	
	@JsonIgnore
	private static TrackingDto instance;

	public static TrackingDto getInstance() {
		if (instance == null) {
			instance = new TrackingDto();
		}
		return instance;
	}
	
	@JsonIgnore
	private Long pId;
	@JsonIgnore
	private Long frUser;
	@JsonIgnore
	private Long toUser;
	@JsonIgnore
	private String action;
	@JsonIgnore
	private String handleStatus;
	@JsonIgnore
	private Date deadine;
	@JsonIgnore
	private String comment;
	@JsonIgnore
	private Integer progress;
	@JsonIgnore
	private int step;
	@JsonIgnore
	private Long node;
	@JsonIgnore
	private String delegateInfo;
	@JsonIgnore
	private String frInfo;
	@JsonIgnore
	private String toInfo;
	
	public TrackingDataDto getData() {
		return new TrackingDataDto(this);
	}
	
	private List<TrackingDto> children;
	public TrackingDto cast(DocumentInProcess p, boolean readToDoing) {
		TrackingDto dto = new TrackingDto();
		dto.setNode(p.getNode());
		dto.setPId(p.getId());
		dto.setFrUser(p.getFrUser());
		dto.setFrInfo(setInfo(p.getFrUsers()));
		dto.setStep(p.getStep());
		dto.setComment(p.getComment());
		dto.setProgress(getProgress(p.getProgress(), p.getHandleStatus()));
		dto.setAction(BussinessCommon.getAction(p.getHandleStatus(), readToDoing, p.getDocument().getStatus()));
		dto.setHandleStatus(BussinessCommon.getTypeEnum(p.getHandleType(), p.getDirection()));
		dto.setToUser(p.getToUser());
		
		boolean delegate = isDelegate(p);
		User to = delegate ? p.getDelegater() : p.getToUsers();
		dto.setToInfo(setInfo(to));
		dto.setDelegateInfo(delegateInfo(delegate, p.getToUsers()));
		dto.setChildren(new ArrayList<>());
		return dto;
	}
	
	private Integer getProgress(Integer progress, DocumentInHandleStatusEnum status) {
		if (DocumentInProcessService.isStatus(status, DocumentInProcessService.NOT_IN_DONE)) {
			return 100;
		}

		if (progress == null)
			return 0;

		return progress;
	}
	
	private final String SPACE = " - ";
	private String delegateInfo(boolean delegate, User u) {
		if (!delegate) return "";
		return "Uỷ quyền bởi : " + setInfo(u);
	}
	
	private String setInfo(User u) {
		return u.getFullName() + SPACE + u.getPositionModel().getName() + SPACE + u.getOrgModel().getName();
	}
	
	private boolean isDelegate(DocumentInProcess p) {
		return p.getStep() > 1 && p.getDelegaterId() != null 
//				&& p.getUpdateBy().equals(p.getDelegaterId())
				&& (p.getDelegater() != null);
	}
	
	public List<TrackingDto> castToList(List<DocumentInProcess> pList, boolean readToDoing) {
		List<TrackingDto> tList = new ArrayList<>();
		pList.forEach(i-> tList.add(cast(i, readToDoing)));
		return tList;
	}
}
