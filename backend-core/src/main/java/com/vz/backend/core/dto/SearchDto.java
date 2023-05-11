package com.vz.backend.core.dto;

import org.springframework.data.domain.Sort.Direction;

import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.SortBy;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class SearchDto {
	private int page = 1;
	private Direction direction = Direction.DESC;
	private int size = Constant.DEFAULT_PAGE_SIZE_INT;
	private SortBy sortBy = SortBy.CREATEDATE;
}
