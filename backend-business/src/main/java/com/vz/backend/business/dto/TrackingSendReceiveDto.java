package com.vz.backend.business.dto;

import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.business.domain.WordEditorProcess;
import com.vz.backend.util.DateTimeUtils;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class TrackingSendReceiveDto {
	private String position;
	private String fullName;
	private Date date;
	private List<TrackingReceiveDocDto> receiveList;
	private List<TaskAttachment> attachments;
	private String content;
	private boolean creator;
	
	public TrackingSendReceiveDto(WordEditorProcess i, boolean creator) {
		super();
		this.setDate(DateTimeUtils.getLastDate(i.getCreateDate(), i.getUpdateDate()));
		this.setFullName(i.getFrUser().getFullName());
		this.setPosition(i.getFrUser().getPositionModel().getName());
		this.setContent(i.getContent());
		this.creator = creator;
	}
	
}
