package com.vz.backend.business.dto;

import com.vz.backend.core.dto.ListObjectDto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class ListNotificationDto extends ListObjectDto<NotificationDto> {
	private Long totalUnread;
}
