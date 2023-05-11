package com.vz.backend.core.config;

public enum NotificationHandleStatusEnum {
	DA_NHAN("Đã nhận"),
	DU_THAO("Dự thảo"),
	DA_TRINH_KY("Đã trình ký"),
	CHO_Y_KIEN("Chờ cho ý kiến"),
	XIN_Y_KIEN("Xin ý kiến"),
	DA_Y_KIEN("Đã cho ý kiến"),
	CHO_XU_LY("Chờ xử lý"),
	CHO_XU_LY_UQ("Ủy quyền - Chờ xử lý"),
	DA_XU_LY("Đã xử lý"),
	DA_THU_HOI("Đã bị thu hồi"),
	DA_TRA_LAI("Đã trả lại"),
	DA_BAN_HANH("Đã ban hành"),
	CHO_BAN_HANH("Chờ ban hành"),
	BI_TRA_LAI("Đã bị trả lại"),
	BI_TRA_LAI_UQ("Ủy quyền - Đã bị trả lại"),
	MOI_DEN("Mới đến"),
	HOAN_THANH("Hoàn thành"),
	NHAN_DE_BIET("Nhận để biết"),
	XU_LY_CHINH("Xử lý chính"),
	XU_LY_CHINH_UQ("Ủy quyền - Xử lý chính"),
	PHOI_HOP("Phối hợp"),
	PHOI_HOP_UQ("Ủy quyền - Phối hợp"),
	DA_THU_HOI_BH("Đã thu hồi"),
	KHOI_PHUC("Khôi phục"),
	CHO_TIEP_NHAN("Chờ tiếp nhận"),
	TU_CHOI_TIEP_NHAN("Từ chối tiếp nhận"),
	DV_HOAN_THANH("Đơn vị hoàn thành"),
	DG_CHAP_NHAN("Đánh giá chấp nhận"),
	DG_TU_CHOI("Đánh giá từ chối"),
	XIN_DG("Xin đánh giá"),
	XIN_DG_UQ("Ủy quyền - Xin đánh giá"),
	CV_XU_LY_CHINH("Xử lý chính"),
	CV_PHOI_HOP("Phối hợp"),
	CV_XLC_TU_CHOI("Từ chối duyệt"),
	CV_PH_TU_CHOI("Từ chối duyệt"),
	CV_DA_GIAO_HOAN_THANH("Hoàn thành công việc"),
	CV_DA_GIAO_CHAP_NHAN("Đã nhận công việc"),
	CV_DA_GIAO_TU_CHOI("Từ chối công việc"),
	CV_DA_DONG_PHOI_HOP("Công việc xử lý phối hợp được đóng"),
	CV_DA_DONG_XU_LY_CHINH("Công việc xử lý chính được đóng"),
	CAL_YC_DUYET_BAN("Chờ duyệt"),
	CAL_TU_CHOI_BAN("Từ chối"),
	CAL_DONG_Y_BAN("Đồng ý"),
	CAL_HUY_DUYET_BAN("Hủy duyệt"),
	CAL_CAP_NHAT_BAN("Cập nhật"),
	CAL_YC_DUYET_CVV("Chờ duyệt"),
	CAL_TU_CHOI_CVV("Từ chối"),
	CAL_DONG_Y_CVV("Đồng ý"),
	CAL_HUY_DUYET_CVV("Hủy duyệt"),
	CAL_CAP_NHAT_CVV("Cập nhật"),
	HS_DUYET_PB("Hồ sơ phòng ban chờ duyệt"),
	HS_DONG_Y_PB("Hồ sơ phòng ban được duyệt"),
	HS_TRA_LAI_PB("Hồ sơ phòng ban bị trả lại"),
	HS_DUYET_CQ("Hồ sơ cơ quan chờ duyệt"),
	HS_DONG_Y_CQ("Hồ sơ cơ quan được duyệt"),
	HS_TRA_LAI_CQ("Hồ sơ cơ quan bị trả lại"),
	HS_CV_CHO_DUYET("Hồ sơ công việc chờ duyệt"),
	HS_CV_DA_DUYET("Hồ sơ công việc được duyệt"),
	HS_CV_CHIA_SE("Hồ sơ được chia sẻ"),
	HS_CV_DUNG_CHIA_SE("Hồ sơ dừng chia sẻ"),
	NOP_LUU_DA_DUYET("Hồ sơ nộp lưu đã được duyệt"),
	NOP_LUU_TU_CHOI("Hồ sơ nộp lưu bị từ chối"),
	CHI_DAO("Được chỉ đạo"),
	NULL2(""),
	
	// Văn bản nội bộ
	NB_CHO_DUYET("Chờ duyệt"),
	NB_DA_DUYET("Đã duyệt"),
	NB_TU_CHOI("Trả lại"),
	NB_THUC_HIEN("Thực hiện"),
	NULL(null),
	
	//Phiên họp (trạng thái cho LOI_MOI_THAM_GIA)
	DON_VI("Đơn vị"),
	CA_NHAN("Cá nhân"),
	DA_GUI("Đã gửi lời mời"),
	THAM_GIA("Tham gia"),
	BAO_VANG("Báo vắng"),
	TU_CHOI("Từ chối tham gia"),
	PHAN_CONG("Đã phân công"),
	CHO_XAC_NHAN("Chờ xác nhận"),
	
	// Dự thảo
	XIN_GOP_Y("Xin góp ý"), 
	DA_GOP_Y("Đã góp ý"), 
	XIN_PHE_DUYET("Xin phê duyệt"), 
	DA_PHE_DUYET("Đã phê duyệt"),
	TU_CHOI_PHE_DUYET("Từ chối phê duyệt"),
	HOAN_THANH_GOP_Y("Hoàn thành góp ý"),
	;

	private final String name;

	NotificationHandleStatusEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}
}
