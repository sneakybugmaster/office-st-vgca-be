package com.vz.backend.business.dto.hstl.ecm;

import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Getter
public class FolderStatusListDto {
	private List<FolderStatusDto> data;
	private Date ngayXuLy;
	private String userNameXuLY;
	private String lyDo;
}
