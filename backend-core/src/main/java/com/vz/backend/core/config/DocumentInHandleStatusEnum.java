package com.vz.backend.core.config;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.core.common.BussinessCommon;

public enum DocumentInHandleStatusEnum {
	MOI_TAO("Mới tạo"), 
	MOI_NHAN("Mới nhận"), 
	CHO_XU_LY("Chờ xử lý"), 
	DANG_XU_LY("Đang xử lý"), 
	DA_XU_LY("Đã xử lý"),
	DA_XU_LY_SWITCH("Chuyển xử lý chính"),
	DA_XU_LY_ADD_USER("Thêm người xử lý"),
	THU_HOI_HOAN_THANH("Thu hồi hoàn thành"), //khi xử lý chính hoàn thành mà muốn thu hồi để hoàn thành lại
	DA_XU_LY_UQ("Ủy quyền - Đã xử lý"), 
	DA_TRA_LAI("Đã trả lại"), 
	DA_TRA_LAI_UQ("Ủy quyền - Đã trả lại"),
	DA_THU_HOI("Đã thu hồi"), 
	CHUYEN_DON_VI("Chuyển đơn vị"), 
	CHUYEN_DON_VI_UQ("Ủy quyền - Chuyển đơn vị"),
	CHO_DANH_GIA("Chờ đánh giá"),
	XIN_DANH_GIA("Xin đánh giá"),
	DG_CHAP_NHAN("Đánh giá chấp nhận"),
	DG_TU_CHOI("Đánh giá từ chối"),
	CHO_CHO_Y_KIEN("Chờ cho ý kiến"),
	DA_CHO_Y_KIEN("Đã cho ý kiến"),
	HOAN_THANH("Hoàn thành"),
	NULL("NULL")
	;

	private final String name;

	DocumentInHandleStatusEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public static DocumentInHandleStatusEnum getEnum(String name) {
		for (DocumentInHandleStatusEnum v : values()) {
			if (v.name().equals(name)) {
				return v;
			}
		}
		return null;
	}

	public static List<DocumentInHandleStatusEnum> getHandleStatusByName(String... status) {
		if (BussinessCommon.isEmptyArr(status)) {
			return null;
		}
		List<DocumentInHandleStatusEnum> handleStatusList = new ArrayList<>();
		for (String st : status) {
			if (DocumentInHandleStatusEnum.getEnum(st) != null) {
				handleStatusList.add(DocumentInHandleStatusEnum.getEnum(st));
			}
		}
		return handleStatusList;
	}
}