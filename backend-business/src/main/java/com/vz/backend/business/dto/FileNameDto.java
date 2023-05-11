package com.vz.backend.business.dto;

import java.util.ArrayList;
import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Getter
public class FileNameDto {
	private List<String> encFileNames = new ArrayList<>();
	private List<String> nonEncFileNames = new ArrayList<>();
	private Long cmtId = null; // cmt after saving for rollback
}
