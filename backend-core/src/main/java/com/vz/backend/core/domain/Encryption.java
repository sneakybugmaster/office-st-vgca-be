package com.vz.backend.core.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.vz.backend.core.common.BussinessCommon;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "ENCRYPTION", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "key", "encrypt", "userId", "client_id" })})
@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class Encryption extends BaseModel {
	@Column(name = "[key]", length = 2048)
	private String key;
	@Column(length = 1024)
	private String encrypt;
	private Long userId;

	@Override
	public void valids() {
		BussinessCommon.require("Khóa", this.key);
		BussinessCommon.require("Tệp mã hóa", this.encrypt);
		BussinessCommon.require("Tài khoản người dùng", this.userId);
	}
}
