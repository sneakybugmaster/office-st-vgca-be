package com.vz.backend.business.config;

import lombok.Getter;

@Getter
public enum DocInternalTrackingEnum {
	CREATE("Tạo mới"),
	UPDATE("Cập nhật"),
	DELETE("Xóa"),
	READ("Xem"),
	DOWNLOAD_FILE("Tải file đính kèm"),
	DONG_Y_DUYET("Đồng ý duyệt"),
	TU_CHOI_DUYET("Từ chối duyệt"),
	TRANSFER("Chuyển xử lý"),
	
	
	THU_HOI_BH("Thu hồi"),
	KHOI_PHUC_BH("Khôi phục"),
	REJECT("Trả lại"),
	INCOMING("Mới đến"),
	FINISH("Hoàn thành");

	private final String name;

	DocInternalTrackingEnum(String name) {
		this.name = name;
	}
}
