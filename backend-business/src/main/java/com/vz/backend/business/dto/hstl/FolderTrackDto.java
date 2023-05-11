package com.vz.backend.business.dto.hstl;

import java.util.Date;

import com.vz.backend.business.config.HsFolderTrackingEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class FolderTrackDto {
	private String personName;
	private Date updateDate;
	private String comment;
	private HsFolderTrackingEnum action;
	
	public String getActionName() {
		return action.getValue();
	}
	
	public FolderTrackDto(String personName, Date updateDate, String comment) {
		super();
		this.personName = personName;
		this.updateDate = updateDate;
		this.comment = comment;
	}

	public FolderTrackDto(String personName, Date updateDate, String comment, HsFolderTrackingEnum action) {
		super();
		this.personName = personName;
		this.updateDate = updateDate;
		this.comment = comment;
		this.action = action;
	}
}
