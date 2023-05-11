package com.vz.backend.business.dto;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ActionControlDto {
		private Long docId;
		private Boolean canRetake;
		private Boolean importDocBook;
		private Boolean canFinish;
		private Boolean canReject;
		private Boolean canConsult; // xin ý kiến
		
		public ActionControlDto(Long docId) {
			this.docId = docId;
		}
}
