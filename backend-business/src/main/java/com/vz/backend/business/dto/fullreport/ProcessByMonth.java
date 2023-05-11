package com.vz.backend.business.dto.fullreport;

import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.HandleTypeEnum;

import lombok.Getter;

public class ProcessByMonth {
	@Getter
	private String key;
	@Getter
	private int year;
	@Getter
	private int part;

	private Set<Long> inNotYet = new HashSet<>();
	private Set<Long> inDone = new HashSet<>();
	private Set<Long> inReject = new HashSet<>();
	private Set<Long> inOverDue = new HashSet<>();
	private Set<Long> outNotYet = new HashSet<>();
	private Set<Long> outDone = new HashSet<>();
	private Set<Long> outReject = new HashSet<>();
	private Set<Long> taskNotYet = new HashSet<>();
	private Set<Long> taskDone = new HashSet<>();
	private Set<Long> taskReject = new HashSet<>();
	private Set<Long> taskOverDue = new HashSet<>();
	
	private Set<Long> inXLC = new HashSet<>(); // Xử lý chính.
	private Set<Long> inXLPH = new HashSet<>(); // Xử lý phối hợp.
	private Set<Long> inNDB = new HashSet<>(); // Nhận để biết.
	private Set<Long> inCD = new HashSet<>(); // Chỉ đạo.

	public int getInNotYet() {
		return inNotYet.size();
	}

	public int getInDone() {
		return inDone.size();
	}

	public int getInReject() {
		return inReject.size();
	}

	public int getInOverDue() {
		return inOverDue.size();
	}

	public int getOutNotYet() {
		return outNotYet.size();
	}

	public int getOutDone() {
		return outDone.size();
	}

	public int getOutReject() {
		return outReject.size();
	}

	public int getTaskNotYet() {
		return taskNotYet.size();
	}

	public int getTaskDone() {
		return taskDone.size();
	}

	public int getTaskReject() {
		return taskReject.size();
	}

	public int getTaskOverDue() {
		return taskOverDue.size();
	}
	
	public int getInXLC() {
		return inXLC.size();
	}
	
	public int getInXLPH() {
		return inXLPH.size();
	}
	
	public int getInNDB() {
		return inNDB.size();
	}
	
	public int getInCD() {
		return inCD.size();
	}
	
	public ProcessByMonth(ReportKey key) {
		this.key = key.getKey();
		this.year = key.getYear();
		this.part = key.getPart();
	}

	public void add(SimpleProcessIn simple) {
		List<DocumentInHandleStatusEnum> doneStatus = Arrays.asList(DocumentInHandleStatusEnum.DA_XU_LY,
				DocumentInHandleStatusEnum.DA_XU_LY_UQ, DocumentInHandleStatusEnum.DA_TRA_LAI,
				DocumentInHandleStatusEnum.DA_TRA_LAI_UQ, DocumentInHandleStatusEnum.DA_THU_HOI,
				DocumentInHandleStatusEnum.CHUYEN_DON_VI);

		if (simple.getDeadline() != null && new Date().compareTo(simple.getDeadline()) > 0
				&& !doneStatus.contains(simple.getHandleStatus())) {
			this.inOverDue.add(simple.getDocId());
		}

		switch (simple.getHandleStatus()) {
		case MOI_TAO:
		case MOI_NHAN:
		case CHO_XU_LY:
		case CHO_DANH_GIA:
		case DANG_XU_LY:
		case XIN_DANH_GIA:
		case DG_CHAP_NHAN:
		case DG_TU_CHOI:
			if (HandleTypeEnum.MAIN.equals(simple.getHandleType())) {
				this.inXLC.add(simple.getDocId());
			}
			if (HandleTypeEnum.SUPPORT.equals(simple.getHandleType())) {
				this.inXLPH.add(simple.getDocId());
			}
			if (HandleTypeEnum.SHOW.equals(simple.getHandleType())) {
				this.inNDB.add(simple.getDocId());
			}
			if (HandleTypeEnum.DIRECTION.equals(simple.getHandleType())) {
				this.inCD.add(simple.getDocId());
			}
			this.inNotYet.add(simple.getDocId());
			break;
		case DA_XU_LY:
		case DA_XU_LY_UQ:
		case CHUYEN_DON_VI:
		case CHUYEN_DON_VI_UQ:
			this.inDone.add(simple.getDocId());
			break;
		case DA_TRA_LAI:
		case DA_TRA_LAI_UQ:
		case DA_THU_HOI:
			this.inReject.add(simple.getDocId());
			break;
		default:
			break;
		}

	}

	public void add(SimpleProcessOut simple) {
		switch (simple.getHandleStatus()) {
		case DU_THAO:
		case CHO_Y_KIEN:
		case CHO_XU_LY:
		case BI_TRA_LAI:
			this.outNotYet.add(simple.getDocId());
			break;
		case DA_TRINH_KY:
		case DA_Y_KIEN:
		case DA_XU_LY:
		case DA_XU_LY_UQ:
			this.outDone.add(simple.getDocId());
			break;
		case DA_THU_HOI:
		case BI_THU_HOI:
		case DA_TRA_LAI:
		case DA_TRA_LAI_UQ:
		case THU_HOI_BH:
			this.outReject.add(simple.getDocId());
			break;
		default:
			break;
		}
	}

	//0.mới giao, 1.đang thực hiện, 2.từ chối, 3.hoàn thành(chờ đánh giá), 4.close
	public void add(SimpleProcessTask simple) {
		if(simple.getDeadline() != null && new Date().compareTo(simple.getDeadline()) > 0 && simple.getApproveStatus() != 4) {
			this.taskOverDue.add(simple.getDocId());
		}

		switch (simple.getApproveStatus()) {
		case 0:
			this.taskNotYet.add(simple.getDocId());
			break;
		case 1:
			this.taskNotYet.add(simple.getDocId());
			break;
		case 2:
			this.taskReject.add(simple.getDocId());
			break;
		case 3:
			this.taskNotYet.add(simple.getDocId());
			break;
		case 4:
			this.taskDone.add(simple.getDocId());
		break;
		default:
			break;
		}
	}
}
