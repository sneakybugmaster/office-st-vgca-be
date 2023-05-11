package com.vz.backend.business.dto.outsideconnect;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.OutsideSystem;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class RejectDataDto {
	private OutsideSystem sys;
	private String orgName;
	private Long orgId;
	private Long objId; // DocId của đơn vị gửi (hệ thống gửi)

	public void valids() {
		BussinessCommon.require("Hệ thống ngoài", this.sys);
		BussinessCommon.require("Tổ chức nhận", this.orgName);
		BussinessCommon.require("Tổ chức nhận", this.orgId);
		BussinessCommon.require("Mã định danh đối tượng", this.objId);
		this.sys.valids();
	}

	public RejectDataDto(OutsideSystem sys, Long objId) {
		super();
		this.sys = sys;
		this.sys.setFrDomain(BussinessCommon.getDomain());
		this.orgName = BussinessCommon.getUser().getOrgModel().getName();
		this.objId = objId;
		this.orgId = BussinessCommon.getUser().getOrg();
	}
}
