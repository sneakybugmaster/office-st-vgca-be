package com.vz.backend.business.config;

import lombok.Getter;

@Getter
public enum DocInternalApproveStatusEnum {
	CHO_DUYET("Chờ duyệt"), DA_DUYET("Đã duyệt"), TRA_LAI("Trả lại"), BINH_LUAN("Bình luận");

	private final String name;
	
	DocInternalApproveStatusEnum(String name) {
		this.name = name;
	}
}