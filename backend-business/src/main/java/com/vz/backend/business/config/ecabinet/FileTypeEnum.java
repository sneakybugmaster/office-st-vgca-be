package com.vz.backend.business.config.ecabinet;

import lombok.Getter;

@Getter
public enum FileTypeEnum {

	// Loại tài liệu
	INVITATION("Loại giấy mời"), SCHEDULE("Chương trình họp"), AGENDA("Nội dung họp"), AGENDA_ITEM("Nội dung họp dự thảo"), MEETING("Phiên họp"), 
	MEETING_DRAFT("Dự thảo phiên họp"), COMMENT_DRAFT("Góp ý dự thảo"),

	// Trạng thái tài liệu
	AVAILABLE("Có sẵn"), ADDITIONAL("Bổ sung"),

	// Xét duyệt
	WAITING("Chờ xét duyệt"), APPROVE("Đã Duyệt"), REJECT("Trả lại"), DRAFT("Lưu nháp");

	private String name;

	FileTypeEnum(String name) {
		this.name = name;
	}
}
