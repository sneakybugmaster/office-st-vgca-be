package com.vz.backend.business.dto.outsideconnect;

import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.OutsideSystem;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class DataRequestOutside {
	private OutsideSystemDto sys;
	private Content content;
	
	public void valids() {
		if(content == null || sys == null) {
			throw new RestExceptionHandler(Message.NO_INPUT_CONNECT_SYSTEM);
		}
		sys.valids();
	}
	
	public OutsideSystem getOutSideSystem() {
		return new OutsideSystem(sys.getDomain(), sys.getName(), null, null, null, null);
	}
}
