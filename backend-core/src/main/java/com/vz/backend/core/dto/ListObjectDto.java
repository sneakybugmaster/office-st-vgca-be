package com.vz.backend.core.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ListObjectDto<T> {
	private long totalPage;
	private long totalRecord;
	private List<T> objList;
}
