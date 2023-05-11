package com.vz.backend.business.dto.document;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
public class ButtonDto {
	/**
	 * Chuyển đơn vị
	 */
	private boolean canFinish;
	private boolean canRetake;
	private boolean canReview;
	private boolean canRequestReview;
	private boolean canAsk;
	private boolean canReply;
	private boolean canRetakeDone;
	private boolean canTransfer;
	private boolean canReturn;
	private boolean canDone;
	private boolean canSwitchOrAdd;
	/**
	 * Luồng tạm ngưng
	 */
	private boolean bpmnError;

	/**
	 * Chuyển xử lý đơn vị
	 */
	private boolean canOrgTransfer;

	/**
	 * Đã xem
	 */
	private boolean canRead;
	/**
	 * Gia hạn xử lý
	 */
	private boolean allowConfig;

	public void setCanRetake(Boolean retake) {
		if (retake == null) {
			this.canRetake = false;
		}
		this.canRetake = retake;
	}
}
