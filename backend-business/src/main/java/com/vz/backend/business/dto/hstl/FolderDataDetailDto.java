package com.vz.backend.business.dto.hstl;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FolderDataDetailDto {
	private List<FolderDetailDto> listFolder;
	private List<IconDetailDto> listIcon;
}
