package com.vz.backend.business.dto.outsideconnect;

import java.util.List;

import org.apache.commons.lang.ArrayUtils;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Encryption;
import com.vz.backend.core.domain.OutsideSystem;
import com.vz.backend.core.dto.IdName;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class OutsideSystemDto {
	public enum TYPE {
		ORG, CERT, ENCRYPT
	}

	private Long outsideId;
	private List<IdName> orgs;
	private String name;
	private String domain;
	private String key;
	private String token;
	private String orgSender = BussinessCommon.getUser().getOrgModel().getName();
	private String frDomain;
	private Long[] userIds;
	private TYPE type;
	private List<Encryption> encryptions;

	public void valids() {
		BussinessCommon.require("Hệ thống liên kết", this.outsideId);
//		BussinessCommon.require("Loại đối tượng dữ liệu nhận", this.type);
		if (this.type == null) {
			type = TYPE.ORG;
		}
		switch (type) {
		case ORG:
			if (BussinessCommon.isEmptyList(orgs))
				throw new RestExceptionHandler(Message.NO_INPUT_CONNECT_SYSTEM);
			orgs.forEach(i -> i.valids());
			break;
		case CERT:
			if (ArrayUtils.isEmpty(userIds))
				throw new RestExceptionHandler(Message.NO_INPUT_CONNECT_SYSTEM);
			break;
		case ENCRYPT:
			if (BussinessCommon.isEmptyList(encryptions))
				throw new RestExceptionHandler(Message.NO_INPUT_CONNECT_SYSTEM);
			encryptions.forEach(i -> i.valids());
			break;
		default:
			break;
		}
	}

	public void set(OutsideSystem os, TYPE type) {
		this.outsideId = os.getId();
		this.name = os.getName();
		this.domain = os.getDomain();
		this.key = os.getKey();
		this.token = os.getToken();
		this.frDomain = BussinessCommon.getDomain();
		this.type = type;
	}
}
