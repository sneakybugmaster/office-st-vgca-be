package com.vz.backend.business.domain.standard;

import javax.persistence.Column;
import javax.persistence.MappedSuperclass;
import javax.persistence.PrePersist;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;

import lombok.Data;
@MappedSuperclass
@Data
public class BaseCode extends BaseModel {

	/**
	 * Mã cơ quan lưu trữ lịch sử - độ dài 13
	 */
	@Column(name = "identifier")
	private String identifier;
	
	/**
	 * Mã phông/công trình/sưu tập lưu trữ  - độ dài 13
	 */
	@Column(name = "organ_ld")
	private String organld;

	@Override
	public void valids() {
		BussinessCommon.require("Mã cơ quan lưu trữ lịch sử", this.identifier);
		BussinessCommon.require("Mã phông/công trình/sưu tập lưu trữ", this.organld);
		BussinessCommon.validLengthData(this.identifier, "Mã cơ quan lưu trữ lịch sử", 13);
		BussinessCommon.validLengthData(this.organld, "Mã phông/công trình/sưu tập lưu trữ", 13);
	}
	
	@PrePersist
	public void prePersit() {
		if(this.identifier == null) {
			this.identifier = BussinessCommon.getUser().getOrgModel().getIdentifier();
		}
	}
}
