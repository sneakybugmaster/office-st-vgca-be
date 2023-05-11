package com.vz.backend.business.dto.hstl;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.vz.backend.business.config.HsFolderProcessEnum;
import com.vz.backend.business.config.HsFolderStatusEnum;
import com.vz.backend.business.service.hstl.HsFolderService;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public class ReportMenuDto {
	@JsonProperty("HSTL_CONGVIEC")
	private long recordsTask;

	@JsonProperty("HSTL_CANHAN")
	private long recordsDoc;

	@JsonProperty("HSTL_PHONGBAN_CHOXACNHAN")
	private long recordsDepConfirm;
	@JsonProperty("HSTL_PHONGBAN_NOPLUUTRU")
	private long recordsDepSubmit;
	@JsonProperty("HSTL_PHONGBAN_TRALAI")
	private long recordsDepReturn;

	@JsonProperty("HSTL_COQUAN_TIEPNHAN")
	private long recordsOrgReceive;
	@JsonProperty("HSTL_COQUAN_DUYETTRALAI")
	private long recordsOrgApproveReturn;
	@JsonProperty("HSTL_COQUAN_TRALAI")
	private long recordsOrgReturn;

	@JsonIgnore
	private Long fromUserId = null;
	@JsonIgnore
	private Long toUserId = null;
	@JsonIgnore
	public Long toOrgId = null;
	@JsonIgnore
	private List<HsFolderStatusEnum> folderStatus = null;
	@JsonIgnore
	private List<HsFolderProcessEnum> processStatus = null;
	@JsonIgnore
	private List<ReportProcessDto> pList = null;

	public ReportMenuDto(List<ReportProcessDto> pList) {
		if (pList.isEmpty())
			return;
		
		setRecordsDoc(pList);
		this.recordsTask = setCondition("recordsTask").setPList(pList).count2();
//		this.recordsDepConfirm = setCondition("recordsDepConfirm").setPList(pList).count();
//		this.recordsDepSubmit = setCondition("recordsDepSubmit").setPList(pList).count();
//		this.recordsDepReturn = setCondition("recordsDepReturn").setPList(pList).count();
		this.recordsOrgReceive = setCondition("recordsOrgReceive").setPList(pList).count();
//		this.reccordsOrgApproveReturn = setCondition("reccordsOrgApproveReturn").setPList(pList).count();
//		this.reccordsOrgReturn = setCondition("reccordsOrgReturn").setPList(pList).count();
	}

	public ReportMenuDto setCondition(String type) {
		int menu = 0;
		int tab = 0;
		switch (type) {
		case "recordsTask":
			menu = 1;
			tab = 1;
			break;
		case "recordsDepConfirm":
			menu = 2;
			tab = 1;
			break;
		case "recordsDepSubmit":
			menu = 2;
			tab = 2;
			break;
		case "recordsDepReturn":
			menu = 2;
			tab = 4;
			break;
		case "recordsOrgReceive":
			menu = 3;
			tab = 1;
			break;
		case "recordsOrgApproveReturn":
			menu = 3;
			tab = 2;
			break;
		case "recordsOrgReturn":
			menu = 3;
			tab = 4;
			break;
		}
		HsFolderService sv = new HsFolderService();
		sv.setCondition(menu, tab);
		return new ReportMenuDto(sv);
	}

	private ReportMenuDto setPList(List<ReportProcessDto> pList) {
		this.pList = pList;
		return this;
	}

	public ReportMenuDto(HsFolderService sv) {
		this.folderStatus = sv.getFolderStatus();
		this.fromUserId = sv.getFromUserId();
		this.processStatus = sv.getProcessStatus();
		this.toOrgId = sv.getToOrgId();
		this.toUserId = sv.getToUserId();
	}

	private long count() {
		Set<Long> rs = new HashSet<>();
		for (ReportProcessDto i : pList) {
			if (folderStatus == null || folderStatus.contains(i.getStatus())
					&& (fromUserId == null || fromUserId.equals(i.getFromUserId()))
					&& (toUserId == null || toUserId.equals(i.getToUserId()))
					&& (toOrgId == null || toOrgId.equals(i.getToOrgId()))) {
				rs.add(i.getFId());
			}
		}
		return rs.size();
	}
	
	private long count2() {
		Set<Long> rs = new HashSet<>();
		for (ReportProcessDto i : pList) {
			if (folderStatus == null || folderStatus.contains(i.getStatus())
					&& ((fromUserId == null || fromUserId.equals(i.getFromUserId()))
					|| (toUserId == null || toUserId.equals(i.getToUserId())))
					&& (toOrgId == null || toOrgId.equals(i.getToOrgId()))) {
				rs.add(i.getFId());
			}
		}
		return rs.size();
	}
	
	private void setRecordsDoc(List<ReportProcessDto> pList) {
		List<ReportProcessDto> recordsDocs = pList.stream().filter(i -> ("2").equals(i.getType()))
				.collect(Collectors.toList());
		this.recordsDoc = recordsDocs.size();
		pList.removeIf(i -> recordsDocs.contains(i));
	}
}
