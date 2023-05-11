package com.vz.backend.business.dto;

import java.util.List;

import com.vz.backend.business.domain.DocumentReceive;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class ShowToKnowDto {
	private String comment;
	private Long docId;
	private List<DocumentReceive> listReceive;
}
