package com.vz.backend.business.dto.ecabinet;

import com.vz.backend.business.config.ecabinet.RoleEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class ParticipantMeetingDto {
	private Long meetingId;
	private Long userId;
	private String fullName;
	private RoleEnum role;
}
