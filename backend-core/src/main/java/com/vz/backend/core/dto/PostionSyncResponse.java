package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class PostionSyncResponse {

	private String userPoisitionName;

	private Long userPoisitionId;

}
