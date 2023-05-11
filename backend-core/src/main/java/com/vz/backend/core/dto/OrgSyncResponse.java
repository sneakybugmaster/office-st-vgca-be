package com.vz.backend.core.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class OrgSyncResponse {

	private String name;

	private Long parentId;

	private Long ID;

	private String code;

	private Long siteId;

	private List<OrgSyncResponse> children;
}
