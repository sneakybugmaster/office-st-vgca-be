package com.vz.backend.business.dto;

import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class EncryptDto {
    Boolean result;
    String fileName;
    List<Long> userIds;
}
