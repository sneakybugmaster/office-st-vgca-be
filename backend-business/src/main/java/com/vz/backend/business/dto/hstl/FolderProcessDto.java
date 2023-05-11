package com.vz.backend.business.dto.hstl;

import java.util.Date;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FolderProcessDto {
	private String personName;
	private Date updateDate;
	private String comment;
}
