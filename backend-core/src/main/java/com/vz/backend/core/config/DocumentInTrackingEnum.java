package com.vz.backend.core.config;

public enum DocumentInTrackingEnum {
	CREATE("Tạo mới"), 
	READ("Xem"), 
	READ_UQ("Ủy quyền - Xem"), 
	UPDATE("Cập nhật"), 
	TRANSFER("Chuyển xử lý"),
	TRANSFER_UQ("Ủy quyền - Chuyển xử lý"), 
	INCOMING("Mới đến"), 
	INCOMING_UQ("Ủy quyền - Mới đến"),
	REJECT("Trả lại văn bản"), 
	REJECT_UQ("Ủy quyền - Trả lại văn bản"), 
	RETAKE("Thu hồi văn bản"), 
	RETAKE_UQ("Thu hồi văn bản"),
	RETAKE_DONE("Thu hồi hoàn thành"),
	RETAKE_DONE_UQ("Thu hồi hoàn thành ủy quyền"),
	FINISH("Đã xử lý"),
	SWITCH("Đã chuyển xử lý chính"),
	ADD_USER_HANDLE("Đã thêm người xử lý"),
	FINISH_UQ("Ủy quyền - Đã xử lý"), 
	EXTENSION("Gia hạn"), 
	RECEIVE("Tiếp nhận"),
	REJECT_RECEIVE("Từ chối tiếp nhận"),
	XIN_Y_KIEN("Xin ý kiến"), 
	DA_Y_KIEN("Đã cho ý kiến"),
	DOWNLOAD_FILE("Tải tệp đính kèm"),
	DIRECTION("Thiết lập chỉ đạo"),
	RESTORE("Khôi phục văn bản");

	private final String name;

	DocumentInTrackingEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
