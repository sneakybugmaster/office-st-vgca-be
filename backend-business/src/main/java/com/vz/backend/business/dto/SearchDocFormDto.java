package com.vz.backend.business.dto;

import java.util.List;

import com.vz.backend.core.dto.CategoryInitDto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class SearchDocFormDto {
	List<CategoryInitDto> orgIssuedCategories;
}
