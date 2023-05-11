package com.vz.backend.business.dto.ecabinet;

import com.vz.backend.business.config.ecabinet.ButtonStatusEnum;

import lombok.Data;
import lombok.Setter;

@Data
public class MeetingActionDto {
	/**
	 * Chương trình họp
	 */
	@Setter
	private ButtonStatusEnum agendaInform;
	
	/**
	 * Phân công tham dự
	 */
	@Setter
	private ButtonStatusEnum assignInform;
	/**
	 * Từ chối tham dự
	 */
	@Setter
	private ButtonStatusEnum refuseInform;
	
	/**
	 * Báo vắng
	 */
	private ButtonStatusEnum absentInform;

	/**
	 * Xác nhận tham gia
	 */
	private ButtonStatusEnum attendInform;

	/**
	 * Thư viện họp
	 */
	private ButtonStatusEnum meetingLibrary;
	
	/**
	 * Gửi cuộc họp
	 */
	private ButtonStatusEnum sendMeeting;

	/**
	 * Sửa phiên họp
	 */
	private ButtonStatusEnum editMeeting;

	/**
	 * Xóa phiên họp
	 */
	private ButtonStatusEnum deleteMeeting;

	/**
	 * Đóng phiên họp
	 */
	private ButtonStatusEnum closeMeeting;

	/**
	 * Ghi âm cuộc họp
	 */
	private ButtonStatusEnum recordMeeting;

	/**
	 * Kết luận phiên họp
	 */
	private ButtonStatusEnum concludeMeeting;

	/**
	 * Diễn biến phiên họp
	 */
	private ButtonStatusEnum courseMeeting;

	/**
	 * Thêm tài liệu từ cuộc họp
	 */
	private ButtonStatusEnum addDocument;

	/**
	 * Truy cập màn hình lớn
	 */
	private ButtonStatusEnum accessIndexPage;
	
	/**
	 * Kết thúc phiên họp
	 */
	private ButtonStatusEnum doneMeeting;
	
	/**
	 * Button Tạo dự thảo phiên họp
	 */
	private ButtonStatusEnum addDraft;
	
	/**
	 * Button Xem dự thảo phiên họp
	 */
	private ButtonStatusEnum viewDraft;
	
	/**
	 * Xác nhận chuyên viên/ thành viên tham gia
	 */
	@Setter
	private Boolean isOwner;
	
	public MeetingActionDto() {
		ButtonStatusEnum button = ButtonStatusEnum.HIDDEN;
		this.absentInform = button;
		this.attendInform = button;
		this.meetingLibrary = button;
		this.editMeeting = button;
		this.deleteMeeting = button;
		this.closeMeeting = button;
		this.recordMeeting = button;
		this.concludeMeeting = button;
		this.courseMeeting = button;
		this.addDocument = button;
		this.accessIndexPage = button;
		this.sendMeeting = button;
		this.agendaInform = button;
		this.assignInform = button;
		this.refuseInform = button;
		this.doneMeeting = button;
		this.addDraft = button;
		this.viewDraft = button;
		this.isOwner = false;
	}
}
