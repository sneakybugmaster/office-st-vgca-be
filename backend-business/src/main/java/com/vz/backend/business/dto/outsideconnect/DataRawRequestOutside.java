package com.vz.backend.business.dto.outsideconnect;

import java.util.List;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class DataRawRequestOutside {
	private Long objId;
	private DocumentTypeEnum type;
	private List<OutsideSystemDto> outsideIds;
	public void valids() {
		BussinessCommon.require("Khóa đối tượng", this.objId);
		BussinessCommon.require("Loại đối tượng", this.type);
		if (BussinessCommon.isEmptyList(outsideIds))
			throw new RestExceptionHandler(Message.NO_INPUT_CONNECT_SYSTEM);
		outsideIds.forEach(i -> i.valids());
	}
}
