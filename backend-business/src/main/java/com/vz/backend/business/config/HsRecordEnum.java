package com.vz.backend.business.config;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.core.dto.LabelValueDto;

public enum HsRecordEnum {
	
	// Cách thức nộp lưu
	TRUC_TUYEN("Hệ thống đã kết nối liên thông"), TRUC_TIEP("Hệ thống chưa kết nối liên thông"),
	CHUA_XAC_DINH("Chưa xác định"),
	
	// Nguồn nộp lưu
	LTQGII("Trung tâm LTQG II"), LTQGIII("Trung tâm LTQG III"), LTQGIV("Trung tâm LTQG IV"),
	CHUA_NOP_TL("Chưa nộp lưu tài liệu giấy");

	public String name;
	
	HsRecordEnum(String name) {
		this.name = name;
	}
	
	public List<LabelValueDto<String>> get(String type) {
		List<LabelValueDto<String>> data = new ArrayList<>();
		if ("src".equalsIgnoreCase(type)) {
			data.add(new LabelValueDto<>(TRUC_TUYEN.toString(), TRUC_TUYEN.name));
			data.add(new LabelValueDto<>(TRUC_TIEP.toString(), TRUC_TIEP.name));
			data.add(new LabelValueDto<>(CHUA_XAC_DINH.toString(), CHUA_XAC_DINH.name));
		} else if ("way".equalsIgnoreCase(type)) {
			data.add(new LabelValueDto<>(LTQGII.toString(), LTQGII.name));
			data.add(new LabelValueDto<>(LTQGIII.toString(), LTQGIII.name));
			data.add(new LabelValueDto<>(LTQGIV.toString(), LTQGIV.name));
			data.add(new LabelValueDto<>(CHUA_NOP_TL.toString(), CHUA_NOP_TL.name));
		}

		return data;
	}
}
