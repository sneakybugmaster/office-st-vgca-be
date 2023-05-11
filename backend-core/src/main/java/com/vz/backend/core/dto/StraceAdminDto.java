package com.vz.backend.core.dto;

import org.springframework.data.domain.Page;

import com.vz.backend.core.common.BussinessCommon;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class StraceAdminDto {
	private long no;
	private String clientName;
	private long countAccess;

	public static Page<StraceAdminDto> convert(Page<StraceAdminDto> pageRs) {
		if (!BussinessCommon.isEmptyPage(pageRs)) {
			long i = pageRs.getPageable().getOffset() + 1;
			for (StraceAdminDto s : pageRs.getContent()) {
				s.setNo(i++);
			}
		}
		return pageRs;
	}

	public StraceAdminDto(String clientName, long countAccess) {
		this.clientName = clientName;
		this.countAccess = countAccess;
	}
}
