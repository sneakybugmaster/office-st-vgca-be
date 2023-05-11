package com.vz.backend.business.dto.outsideconnect;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class ResultMsg {
	private List<String> successSystem;
	private List<ErrorMsg> failSystem;
}
