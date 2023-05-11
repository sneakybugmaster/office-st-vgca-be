package com.vz.backend.business.config.ecabinet;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.core.dto.LabelValueDto;

public enum MeetingStatusEnum {

	// Phát biểu
	DA_MOI("Đã mời"), THAM_GIA("Tham gia"), DANG_KI_PHAT_BIEU("Đã đăng kí phát biểu"), DANG_PHAT_BIEU("Đang phát biểu"),
	DA_PHAT_BIEU("Đã phát biểu"), GHE_TRONG("Ghế trống"), BAO_VANG("Báo vắng"), THAM_GIA_Y_KIEN("Tham gia ý kiến"),
	CHI_DINH_PHAT_BIEU("Chờ phát biểu"), PHAT_BIEU("Phát biểu"),
	
	// Speech to text
	CHUA_XAC_NHAN("Chưa xác nhận"), DA_XAC_NHAN("Đã xác nhận"),

	// Phiên họp
	MOI_TAO("Chưa gửi"), DA_GUI("Đã gửi"), DANG_HOP("Đang họp"), DONG("Đóng"), DA_HOP("Kết thúc phiên họp"),

	// Điểm danh
	WAITING("Chờ xác nhận"), ATTEND("Tham gia"), ABSENT("Vắng mặt"),
	
	// Biểu quyết
	VOTING("Chờ biểu quyết"), VOTED("Đã biểu quyết");

	public String name;

	MeetingStatusEnum(String name) {
		this.name = name;
	}

	/**
	 * Danh sách Trạng thái đăng kí phát biểu
	 * 
	 * @return
	 */
	public List<MeetingStatusEnum> getSpeechRegister() {
		List<MeetingStatusEnum> rs = new ArrayList<>();
		rs.add(CHI_DINH_PHAT_BIEU);
		rs.add(DANG_KI_PHAT_BIEU);
		rs.add(DANG_PHAT_BIEU);
		rs.add(DA_PHAT_BIEU);
		return rs;
	}
	
	/**
	 * Danh sách Trạng thái đăng kí phát biểu
	 * 
	 * @return
	 */
	public List<MeetingStatusEnum> getSpeechDone() {
		List<MeetingStatusEnum> rs = new ArrayList<>();
		rs.add(DA_PHAT_BIEU);
		return rs;
	}

	/**
	 * Danh sách Trạng thái trong sơ đồ phòng họp
	 * 
	 * @return
	 */
	public List<LabelValueDto<String>> getStatus() {
		List<LabelValueDto<String>> rs = new ArrayList<>();
		rs.add(new LabelValueDto<String>(String.valueOf(THAM_GIA), THAM_GIA.name));
		rs.add(new LabelValueDto<String>(String.valueOf(DANG_KI_PHAT_BIEU), DANG_KI_PHAT_BIEU.name));
		rs.add(new LabelValueDto<String>(String.valueOf(DANG_PHAT_BIEU), DANG_PHAT_BIEU.name));
		rs.add(new LabelValueDto<String>(String.valueOf(DA_PHAT_BIEU), DA_PHAT_BIEU.name));
		rs.add(new LabelValueDto<String>(String.valueOf(GHE_TRONG), GHE_TRONG.name));
		rs.add(new LabelValueDto<String>(String.valueOf(BAO_VANG), BAO_VANG.name));
		rs.add(new LabelValueDto<String>(String.valueOf(DA_MOI), DA_MOI.name));
		return rs;
	}

}
