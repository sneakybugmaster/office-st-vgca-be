package com.vz.backend.business.dto.hstl;

import java.util.List;

import com.vz.backend.core.dto.LabelValueId;

import lombok.AllArgsConstructor;
import lombok.Getter;

@Getter
@AllArgsConstructor
public class CriteriaSearchDto {
	/**
	 * Năm hình thành hồ sơ
	 */
	private List<Integer> yearFolders;
	
	/**
	 * Loại hồ sơ/ Số kí hiệu
	 */
	private List<String> typeFolders;
	
	/**
	 * Thời hạn lưu trữ
	 */
	private List<LabelValueId<String>> maintenances;
}
