package com.vz.backend.business.dto;

import com.vz.backend.business.domain.BusinessTracking.BusinessTrackingType;

import lombok.AllArgsConstructor;
import lombok.Getter;

@AllArgsConstructor
@Getter
public class BusinessTrackingDto {
	public static final String CONSTRUCTOR_OPEN = "new com.vz.backend.business.dto.BusinessTrackingDto("
			+ "t.id, t.docId, t.type, t.count, ";
	public static final String CONSTRUCTOR_CLOSE = ") ";

	private Long id;
	private Long docId;
	private BusinessTrackingType type;
	private long count = 0;
	private String title;
	private String code;

	public BusinessTrackingDto(Long id, Long docId, BusinessTrackingType type, long count, String title, long code, String sub) {
		super();
		this.id = id;
		this.docId = docId;
		this.type = type;
		this.count = count;
		this.title = title;
		this.code = code + "";

		if (sub != null) {
			this.code = this.code + sub;
		}
	}


}
