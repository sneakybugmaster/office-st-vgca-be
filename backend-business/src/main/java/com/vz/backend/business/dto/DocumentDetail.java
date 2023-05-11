package com.vz.backend.business.dto;

import java.util.List;

import com.vz.backend.business.domain.Documents;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@Data
@NoArgsConstructor
@AllArgsConstructor
public class DocumentDetail {
	private DocumentDetailDto document;
	private String bookName;
	private String docTypeName;
	private String urgentName;
	private String securityName;
	private String docFieldsName;
	private String docStatusName;
	private String methodReceiptName;
	private DocumentBasicDto parentDoc;
	private List<DocumentBasicDto> listChildrenDoc;
	private List<DocumentBasicDto> listResponseDoc;
	private List<TaskDto> listTask;
}
