package com.vz.backend.business.dto;

import java.util.List;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.Documents;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class TaskDocumentDto {

	private List<Documents> documentIn;

	private List<DocumentOut> documentOut;
}
