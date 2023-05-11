package com.vz.backend.core.config;

public enum AuthorityEnum {
	APPROVE_UNIT_LEVEL_CALENDAR("Duyệt lịch đơn vị"),
	APPROVE_TOP_LEVEL_CALENDAR("Duyệt lịch BAN"),
	DUYET_HOSO("Duyệt hồ sơ công việc"),
	APPROVE_MEETING_CALENDAR("Duyệt lịch họp"),
	LEADERSHIP("Lãnh đạo CỤC"),
	LEADERSHIP_UNIT("Lãnh đạo PHÒNG"),
	DIRECTION_DOCUMENT("Quyền chỉ đạo"),
	MANAGE_HEADINGS("Quản lý đề mục"),
	REPORT_CHINH_QUYEN("Duyệt báo cáo chính quyền"),
	REPORT_DANG("Duyệt báo cáo đảng"),
	REPORT_VP_BAN("Văn phòng ban"),
	VIEW_STATISTIC_BY_DOC_HANDLE_STATE("Xem thống kê văn bản theo trạng thái xử lý");

	private final String name;

	AuthorityEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}