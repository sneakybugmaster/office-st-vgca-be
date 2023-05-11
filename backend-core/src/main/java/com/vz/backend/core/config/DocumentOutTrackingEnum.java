package com.vz.backend.core.config;

public enum DocumentOutTrackingEnum {
	CREATE("Tạo mới"), 
	READ("Xem"), 
	TRANSFER("Chuyển xử lý"), 
	UPDATE("Cập nhật"), 
	RETAKE("Thu hồi"),
	THU_HOI_BH("Thu hồi"), 
	KHOI_PHUC_BH("Khôi phục"), 
	REJECT("Trả lại"), 
	INCOMING("Mới đến"), 
	FINISH("Hoàn thành"),
	DOWNLOAD_FILE("Tải file"),
	DELETE("Xóa"),
	FORWARD("Chuyển tiếp"),
	HOAN_THANH("Hoàn thành"),
	THU_HOI_HOAN_THANH("Thu hồi hoàn thành"),
	REJECT_RECEIVE("Từ chối tiếp nhận"),
	TRANSFER_OUTSIDE("Nhận văn bản liên thông"),
	;

	private final String name;

	DocumentOutTrackingEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
