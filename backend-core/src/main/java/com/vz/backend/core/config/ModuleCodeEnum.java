package com.vz.backend.core.config;

public enum ModuleCodeEnum {
	PROCESS("PROCESS"), // Luồng
	RETAKE("RETAKE"), // Thu hồi
	DELEGATE("DELEGATE"), // Uỷ quyền
	USER("USER"), // Người dùng
	ORG("ORG"), // Đơn vị
	CATEGORY("CATEGORY"), // Danh mục
	DELEGATE_FLOW("DELEGATE_FLOW"), // Luồng ủy quyền
	DOCUMENT_BOOK("DOCUMENT_BOOK"), // Sổ văn bản

	DOC_OUT_RETAKE("DOC_OUT_RETAKE"), // Thu hồi văn bản đi
	DOC_IN_RETAKE("DOC_IN_RETAKE"), // Thu hồi văn bản đến
	// Văn bản đi
	DOCUMENT_IN("DOCUMENT_IN"), // Văn bản đi
	DRAFT_LIST("DRAFT_LIST"), // Văn bản trình ký
	DRAFT_HANDLE("DRAFT_HANDLE"), // Văn bản xử lý
	DOCUMENT_IN_LIST("DOCUMENT_IN_LIST"), // Danh sách văn bản đi
	DRAFT_ISSUED("DRAFT_ISSUED"), // Văn bản ban hành
	// Văn bản đến
	DOC_OUT_LIST("DOC_OUT_LIST"), // Tiếp nhận văn bản
	DOC_OUT_MAIN("DOC_IN_MAIN"), // Xử lý chính
	DOC_OUT_COMBINE("DOC_IN_SUPPORT"), // Xử lý phối hợp
	DOC_OUT_KNOW("DOC_IN_KNOW"), // Nhận để biết
	DOC_OUT_DIRECTION("DOC_OUT_DIRECTION"), // danh sách chỉ đạo 
	DOC_OUT_DONE("DOC_OUT_DONE"), // Đã xử lý
	DOC_OUT_WAIT("DOC_OUT_WAIT"), // Chờ xử lý
	DOCUMENT_OUT_INTERNAL("DOCUMENT_OUT_INTERNAL"), // Văn bản đến nội bộ
	// Ủy quyền
	DELEGATE_MANAGE("DELEGATE_MANAGE"), // Quản lý ủy quyền
	DOC_IN_DELEGATE("DOC_IN_DELEGATE"), // Văn bản đến ủy quyền
	DOC_OUT_DELEGATE("DOC_OUT_DELEGATE"), // Văn bản đi ủy quyền

	// Theo dõi văn bản
	TRACK_DOC_IN("TRACK_DOC_IN"), // Theo dõi văn bản đến
	TRACK_DOC_OUT("TRACK_DOC_OUT"), // Theo dõi văn bản đi

	// Lich chi Hoa
	CAL_PERSON("CAL_PERSON"),

	// Lịch
	CAL_BUSINESS("CAL_BUSINESS"), // Lịch công tác
	CAL_REGISTER("CAL_REGISTER"), // Lịch đăng kí
	CAL_MEETING("CAL_MEETING"), //Lịch họp

	// SCHEDULE_REMIND
	SCHEDULE_REMIND("SCHEDULE_REMIND"),
	SCHEDULE_REMIND_VIEW("SCHEDULE_REMIND_VIEW"),
	SCHEDULE_REMIND_MANAGE("SCHEDULE_REMIND_MANAGE"),

	//Giao việc
	TASK_MAIN("TASK_MAIN"), //Giao việc- Xử lý chính
	TASK_SUPPORT("TASK_SUPPORT"), //Giao việc- Xử lý phối hợp
	TASK_ASSIGN("TASK_ASSIGN"), //Giao việc- Công việc đã giao
	
	//Hồ sơ tài liệu
	HSTL_CONGVIEC("HSTL_CONGVIEC"),
	HSTL_COQUAN("HSTL_COQUAN"),
	HSTL_PHONGBAN("HSTL_PHONGBAN"),
	HSTL_CANHAN("HSTL_CANHAN"),
	HSTL_NOPLUU("HSTL_NOPLUU"),
	
	//Văn bản nội bộ
	DOC_INTERNAL_PUBLISH("DOC_INTERNAL_PUBLISH"), // Đã ban hành
	DOC_INTERNAL_RETURN("DOC_INTERNAL_RETURN"), // Văn bản trả lại
	DOC_INTERNAL_APPROVE("DOC_INTERNAL_APPROVE"), // Duyệt văn bản
	DOC_INTERNAL_DOING("DOC_INTERNAL_DOING"), // Đang duyệt
	DOC_INTERNAL_WAITING("DOC_INTERNAL_WAITING"), // Chờ duyệt
	DOC_INTERNAL_REGISTER("DOC_INTERNAL_REGISTER"), // Đăng ký phát hành
	DOC_INTERNAL_INCOMING("DOC_INTERNAL_INCOMING"),
	DOC_INTERNAL_ISSUED_INCOMING("DOC_INTERNAL_ISSUED_INCOMING"),
	
	KPI_SETUP("KPI_SETUP"), //thiết lập KPI
	
	//văn bản mẫu
	ADMIN("ADMIN"), // quyền quản trị
	ROLE("ROLE"), //Phân quyền vai trò
	CALENDAR("CALENDAR"), // Quản lý lịch
	MEETING_ROOM("MEETING_ROOM"), // Quản lý phòng họp
	GROUP_CONTACT("GROUP_CONTACT"), // Nhóm người dùng
	
	//Phiên họp
	CABINET_MEETING("CABINET_MEETING"),
	CABINET_DRAFT("CABINET_DRAFT"),

	USER_MANUAL_CONFIG("USER_MANUAL_CONFIG"), // Cấu hình hướng dẫn sử dụng
	;
	
	public final String name;

	ModuleCodeEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
