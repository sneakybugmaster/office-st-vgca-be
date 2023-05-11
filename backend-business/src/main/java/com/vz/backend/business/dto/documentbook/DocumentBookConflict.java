package com.vz.backend.business.dto.documentbook;

import com.vz.backend.core.exception.CustomMessage;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class DocumentBookConflict implements CustomMessage {
	private Long numberInBook;
	private Long numberInMainBook;
	
	public DocumentBookConflict(Long numberInBook, Long numberInMainBook) {
		this.numberInBook = numberInBook;
		this.numberInMainBook = numberInMainBook;
	}

	@Override
	public String message() {
		if (!this.numberInBook.equals(-1L) && !this.numberInMainBook.equals(-1L)) {
			return "Tổng số đã thay đổi.<br>Số văn bản đã được sử dụng";
		}
		if (!this.numberInBook.equals(-1L)) {
			return "Số văn bản đã được sử dụng";
		}
		if (!this.numberInMainBook.equals(-1L)) {
			return "Tổng số đã thay đổi";
		}
		return "Lỗi..........";
	}
	
}
