package com.vz.backend.business.dto;

import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class HandlerDto {
	private User assignHandler;
	private User delegateHandler;
	private boolean lead;
	
	public HandlerDto(User assignHandler) {
		this.assignHandler = assignHandler;
	}
}
