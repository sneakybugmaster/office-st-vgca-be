package com.vz.backend.business.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@AllArgsConstructor
public class DocInRetakeDto {
	private Long id;
	private Date retakeDate;
	private String preview;
	private String orgIssuedName;
	private Long docFieldsId;
	private Long docTypeId;
	private Long numberArrival;
	@JsonIgnore
	private String bookName;
	private String numberOrSign;
	private Date dateIssued;
	private String placeSend;
	private String numberArrivalStr;

	public String getNumberArrivalStr() {
		if (this.numberArrivalStr != null) {
			return this.numberArrivalStr;
		}
		return this.bookName != null ? this.numberArrival + this.bookName : this.numberArrival.toString();
	}
}
