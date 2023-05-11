package com.vz.backend.core.config;

import com.vz.backend.core.common.BussinessCommon;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public enum HandleTypeEnum {
	MAIN, SUPPORT, SHOW, DIRECTION, INTERNAL_INCOMING, INTERNAL_ISSUED_INCOMING, NULL;

	public static List<HandleTypeEnum> getListHandleType(HandleTypeEnum... types) {
		if (BussinessCommon.isEmptyArr(types)) {
			return null;
		}
		return new ArrayList<>(Arrays.asList(types));
	}

}