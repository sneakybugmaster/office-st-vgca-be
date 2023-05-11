package com.vz.backend.business.dto.task;

import com.vz.backend.business.config.ButtonStatusEnum;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class TaskButtonActionDto {

	/**
	 * Chuyển xử lý
	 */
	private ButtonStatusEnum canTransfer;

	/**
	 * Hoàn thành
	 */
	private ButtonStatusEnum canDone;

	/**
	 * Đóng việc
	 */
	private ButtonStatusEnum canClose;
	
	/**
	 * Xóa
	 */
	private ButtonStatusEnum canDelete;
	
	/**
	 * Sửa
	 */
	private ButtonStatusEnum canEdit;

	/**
	 * Thu hồi
	 */
	private ButtonStatusEnum canRevoke;

	/**
	 * Tiếp nhận
	 */
	private ButtonStatusEnum canTodo;

	/**
	 * Từ chối
	 */
	private ButtonStatusEnum canReject;
	
	/**
	 * Từ chối duyệt (sau khi xử lý chính hoàn thành)
	 */
	private ButtonStatusEnum canRejectApprove;
	
	/**
	 * Thu hồi hoàn thành
	 */
	private ButtonStatusEnum canRevokeFinish;
	
	/**
	 * Khôi phục (Sau khi đã thu hồi)
	 */
	private ButtonStatusEnum canRestore;
	
	/**
	 * Giao việc bổ sung
	 */
	private ButtonStatusEnum canAddTransfer;
	
	/**
	 * Kiếm trả Người giao/ Giao việc ở màn hình chi tiết
	 */
	private Boolean assigner = false;
	
	public TaskButtonActionDto(ButtonStatusEnum status) {
		this.canTransfer = status;
		this.canDone = status;
		this.canClose = status;
		this.canRevoke = status;
		this.canTodo = status;
		this.canReject = status;
		this.canDelete = status;
		this.canRejectApprove = status;
		this.canEdit = status;
		this.canRevokeFinish = status;
		this.canRestore = status;
		this.canAddTransfer = status;
	}
}
