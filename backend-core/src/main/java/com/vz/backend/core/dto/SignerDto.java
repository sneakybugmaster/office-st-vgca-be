package com.vz.backend.core.dto;

import java.io.Serializable;
import java.math.BigInteger;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class SignerDto implements Serializable {

	public SignerDto(Object id, String fullName, String orgName, String position, String phone) {
		if (id instanceof Long) {
			this.id = Long.valueOf(id.toString());
		} else {
			this.id = ((BigInteger) id).longValue();
		}
		this.fullName = fullName;
		this.orgName = orgName;
		this.position = position;
		this.phone = phone;
	}

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	int no;
	private Long id;
	private String fullName;
	private String orgName;
	private String position;
	private String phone;
}
