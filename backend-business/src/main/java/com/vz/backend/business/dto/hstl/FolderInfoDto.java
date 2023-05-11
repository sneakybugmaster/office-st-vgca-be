package com.vz.backend.business.dto.hstl;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FolderInfoDto {
	FolderDetailDto folderDetail;
	List<FolderTrackDto> listTrack;
}
