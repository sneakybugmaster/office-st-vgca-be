package com.vz.backend.business.dto.document;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class DocumentReceiveBasicDto {
	private Long id;
	private String type;
	
	public DocumentReceiveBasicDto(Long id, String type) {
		super();
		this.id = id;
		this.type = type;
	}
}
