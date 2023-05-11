package com.vz.backend.core.dto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.List;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class OrgAndUserDto {

    private long child;
    private String name;
    private Long parent;
    private long type; //0:org 1:user
    private Boolean check;
}
