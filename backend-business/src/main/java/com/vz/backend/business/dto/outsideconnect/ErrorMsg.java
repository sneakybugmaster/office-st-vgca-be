package com.vz.backend.business.dto.outsideconnect;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ErrorMsg {
	private String nameSys;
	private List<String> msgError;
}
