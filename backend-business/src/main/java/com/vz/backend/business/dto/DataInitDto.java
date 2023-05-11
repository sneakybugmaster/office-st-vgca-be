package com.vz.backend.business.dto;

import java.util.List;

import com.vz.backend.core.dto.CategoryInitDto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class DataInitDto {
	private String orgCreateName;
	private String userCreateName;
	private Long node;
	private List<CategoryInitDto> docTypeCategories;
	private List<CategoryInitDto> docFieldCategories;
	private List<CategoryInitDto> securityCategories;
	private List<CategoryInitDto> urgentCategories;
	private List<DocumentBookInitDto> bookCategories;
	private Long bookId;
	private boolean checkRoleLibrarian;
}
