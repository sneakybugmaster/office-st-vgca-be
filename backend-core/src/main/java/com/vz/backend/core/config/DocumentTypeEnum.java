package com.vz.backend.core.config;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.vz.backend.core.dto.LabelValueDto;

public enum DocumentTypeEnum {
	VAN_BAN_DEN("Văn bản đến"), VAN_BAN_DI("Văn bản đi"), TAO_LICH("Lịch"), GIAO_VIEC("Giao việc"),
	NHAC_VIEC("Nhắc việc"), VAN_BAN_NOI_BO("Văn bản nội bộ"), VAN_BAN_DEN_NOI_BO("Văn bản đến nội bộ") , VAN_BAN_UY_QUYEN("Văn bản ủy quyền"),
	VAN_BAN_DEN_CMT("Bình luận văn bản đến"), TEP("Tệp"), VAN_BAN_SOAN_THAO("Văn bản soạn thảo"), TU_HO_SO("Tủ hồ sơ"),
	VAN_BAN_MAU("Văn bản mẫu"), KPI("Chỉ số đánh giá công việc"), REPORT_FIELDS_CONFIGS("Cấu hình cột báo cáo"),
	VAN_BAN_DI_DU_THAO("Dự thảo văn bản đi"), VAN_BAN_DI_LIEN_QUAN("Văn bản đi liên quan"),
	VAN_BAN_DI_BINH_LUAN("Văn bản đi bình luận"), GIAO_VIEC_BINH_LUAN("Giao việc bình luận"),
	VAN_BAN_NOI_BO_VAN_BAN("Văn bản nội bộ _ Văn bản"), VAN_BAN_NOI_BO_PHU_LUC("Văn bản nội bộ _ Phụ lục"),
	VAN_BAN_NOI_BO_BINH_LUAN("Văn bản nội bộ _ Bình luận"),

	//Phiên họp
	LOI_MOI_THAM_GIA("Lời mời tham gia"), 
	THANH_PHAN_THAM_GIA("Thành phần tham gia"),
	
	//Dự thảo
	CHUYEN_XU_LY("Chuyển xử lý"),
	PHE_DUYET("Phê duyệt"),
	;
	
	private static final List<DocumentTypeEnum> KPI_HIDES = Arrays.asList(NHAC_VIEC, VAN_BAN_NOI_BO, VAN_BAN_UY_QUYEN,
			VAN_BAN_DEN_CMT, TEP, VAN_BAN_SOAN_THAO, TU_HO_SO, VAN_BAN_MAU, VAN_BAN_DI_DU_THAO, VAN_BAN_DI_LIEN_QUAN,
			VAN_BAN_DI_BINH_LUAN, GIAO_VIEC_BINH_LUAN, VAN_BAN_NOI_BO_VAN_BAN, VAN_BAN_NOI_BO_PHU_LUC);
	private static final List<DocumentTypeEnum> REPORT_FIELDS_HIDES = Arrays.asList(NHAC_VIEC, VAN_BAN_NOI_BO,
			VAN_BAN_UY_QUYEN, VAN_BAN_DEN_CMT, TEP, VAN_BAN_SOAN_THAO, TU_HO_SO, VAN_BAN_MAU, TAO_LICH,
			REPORT_FIELDS_CONFIGS, KPI, VAN_BAN_DI_DU_THAO, VAN_BAN_DI_LIEN_QUAN, VAN_BAN_DI_BINH_LUAN,
			GIAO_VIEC_BINH_LUAN, VAN_BAN_NOI_BO_VAN_BAN, VAN_BAN_NOI_BO_PHU_LUC);

	public final String name;

	DocumentTypeEnum(String name) {
		this.name = name;
	}

	public String getName() {
		return name;
	}

	public static List<LabelValueDto<String>> get(DocumentTypeEnum type) {
		List<LabelValueDto<String>> data = new ArrayList<>();
		for (DocumentTypeEnum v : values()) {
			if (getHides(type).contains(v))
				continue;
			data.add(new LabelValueDto<>(v.toString(), v.name));
		}
		return data;
	}

	private static List<DocumentTypeEnum> getHides(DocumentTypeEnum type) {
		switch (type) {
		case KPI:
			return KPI_HIDES;
		case REPORT_FIELDS_CONFIGS:
			return REPORT_FIELDS_HIDES;
		default:
			return new ArrayList<>();
		}
	}
}
