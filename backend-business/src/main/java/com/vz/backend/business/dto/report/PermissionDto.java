package com.vz.backend.business.dto.report;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


@Data
@NoArgsConstructor
@AllArgsConstructor
public class PermissionDto {
    private boolean isDeleteButton;
    private boolean isEditButton;
    private boolean isApproveButton;
}
