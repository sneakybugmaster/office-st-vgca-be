package com.vz.backend.core.dto;

import lombok.Data;

import java.util.List;

@Data
public class RequestCerOrgAndGroup {
    private List<OrgGroupDto> participantsGroup;
    private List<OrgGroupDto> participantsOrg;
    private List<Long> userIds;
}
