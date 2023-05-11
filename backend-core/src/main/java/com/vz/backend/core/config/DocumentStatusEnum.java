package com.vz.backend.core.config;

public enum DocumentStatusEnum {

	// for document out
	DU_THAO("Dự thảo"), 
	DANG_XU_LY("Đang xử lý"), 
	CHO_BAN_HANH("Chờ ban hành"), 
	DA_BAN_HANH("Đã ban hành"),
	BI_TRA_LAI("Bị trả lại"), 
	THU_HOI_XL("Thu hồi"), 
	THU_HOI_BH("Thu hồi"),

	// for document in
	RETURN_DOC("Trả lại văn bản"), 
	RETAKE_DOC("Thu hồi văn bản"), 
	RETAKE_ORG("Đã thu hồi"), 
	DOING("Đang xử lý"),
	NOT_YET("Chờ xử lý"), 
	WAIT_RECEIVE("Chờ tiếp nhận"),
	REJECT_RECEIVE("Từ chối tiếp nhận"),
	DONE("Hoàn thành"),
	
	// for document internal - Văn bản nội bộ
	NB_DU_THAO("Dự thảo"),
	NB_CHO_DUYET("Chờ cho ý kiến"),
	NB_TRA_LAI("Bị trả lại"),
	NB_LANH_DAO_KY("Chờ lãnh đạo ký"),
	NB_BAN_HANH("Đã cho ý kiến"),
	NB_THU_HOI("Thu hồi"),
	NB_HOAN_THANH("Nơi nhận hoàn thành văn bản"),
	
	// check document delegated
	DELEGATE_DOC("Văn bản ủy quyền"),
	;

	private final String name;

	DocumentStatusEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public static DocumentStatusEnum getEnum(String name) {
		for (DocumentStatusEnum v : values()) {
			if (v.name().equals(name)) {
				return v;
			}
		}
		return null;
	}
}
