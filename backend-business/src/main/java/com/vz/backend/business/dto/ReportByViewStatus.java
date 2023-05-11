package com.vz.backend.business.dto;

import com.vz.backend.business.config.ViewStatusEnum;
import com.vz.backend.core.domain.Organization;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ReportByViewStatus {
    private String chartName;
    private Long orgId;
    private String orgName;
    private Long orgUsersCount;
    private List<Organization> childOrganizations = new ArrayList<>();
    private Map<ViewStatusEnum, Long> documentsCountByViewStatus = new HashMap<>();
    private Map<ViewStatusEnum, String> viewStatusDescription = new HashMap<>();

}
