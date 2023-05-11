package com.vz.backend.business.dto;

import com.vz.backend.core.config.HandleTypeEnum;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
public class ProcessDocumentDto {
	private long docId;
	private long node; 
	private long nextNode;
	private boolean canRetake;
	private boolean canReview;
	private boolean canRequestReview;
	private boolean canAsk;
	private boolean canReply;
	private boolean canRetakeDone;
	private boolean canTransfer;
	/**
	 * Trả lại văn bản
	 */
	private boolean canReturn;
	
	/**
	 * Hoàn thành xử lý
	 * */
	private boolean canDone;
	
	/**
	 * Thêm xử lý (Chỉ áp dụng cho Chuyển xử lý)
	 */
	private boolean canSwitchOrAdd;
	
	/**
	 * Loại xử lý
	 */
	private HandleTypeEnum type;
	
	/**
	 * Chuyển xử lý đơn vị
	 */
	private boolean canOrgTransfer;
	
	/**
	 * Step của process
	 */
	
	private long step;
	/**
	 * Chuyển đơn vị
	 */
	private boolean canFinish;
	
	/**
	 * Gia hạn hạn xử lý
	 */
	private boolean allowConfig;
	
	/**
	 * btn Đã xem (nhận để biết)
	 */
	private boolean canRead;
	
	/**
	 * Hoàn thành xử lý văn bản nội bộ
	 */
	private boolean canDoneInternal;
	
	/**
	 * Văn bản thông luồng
	 */
	private boolean mergedLines;
}