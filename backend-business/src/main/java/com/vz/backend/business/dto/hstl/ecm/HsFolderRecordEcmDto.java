package com.vz.backend.business.dto.hstl.ecm;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.core.common.BussinessCommon;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@NoArgsConstructor
public class HsFolderRecordEcmDto {
	@Setter
	private List<HsFolderRecordDto> data;
	private String callBack;
	private String token;

	public HsFolderRecordEcmDto(Long formId) {
		this.token = BussinessCommon.getToken();
		this.data = new ArrayList<>();
		this.callBack = BussinessCommon.getDomain() + "/api/hstl-form/folders/update/"+ formId;
	}
}
