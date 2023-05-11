package com.vz.backend.business.dto.hstl.ecm;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import com.vz.backend.business.domain.hstl.HsFolderRecordForm;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
public class HsFolderRecordFormDto {
	private String maYeuCau;
	private String noiDung;
	private String ngayTao;
	private Long userCreate;
	private String userName;
	private Long userUpdate;
	private String userNameUpdate;
	private Integer status; // 1: Gửi mới, 2: bổ sung 3: Bổ sung, 4: từ chối, 5: duyệt
	private Long toChucId; // lấy tới tổ chức cấp cục
	private Integer nguonNopLuu;
	private Integer soLanNopLuu;
	private Integer tongSoHoSo;
	private Integer tongSoTrang;
	private Integer tongSoDungLuong;
	private Integer cachThucNopLuu;
	private String thoiGianDuKienNop;
	private String diaChiLienHe;
	private String ghiChu;
	private String token;
	private List<FolderAttachmentDto> dinhKem;
	private String congVanCoQuanToChuc = "";
	private String callBack;
	private String mucLucHoSo = "";
	private String system = "OFFICE";

	public void convert(HsFolderRecordForm form, boolean changeFormat) {
		this.ngayTao = form.getCreateDate() == null
				? DateTimeUtils.convertDateToStringPattern(new Date(), DateTimeUtils.YYYY_MM_DD)
				: DateTimeUtils.convertDateToStringPattern(form.getCreateDate(), DateTimeUtils.YYYY_MM_DD);
		this.userCreate = form.getCreator() == null ? BussinessCommon.getUserId() : form.getCreator().getId();
		this.userName = form.getCreator() == null ? BussinessCommon.getUser().getFullName()
				: form.getCreator().getUserName();
		this.userUpdate = form.getUpdater() == null ? BussinessCommon.getUserId() : form.getUpdater().getId();
		this.userNameUpdate = form.getUpdater() == null ? BussinessCommon.getUser().getUserName()
				: form.getUpdater().getFullName();
		this.status = form.getStatus();
		this.toChucId = form.getOrgId();
		this.nguonNopLuu = form.getSrc() == null ? 0 : form.getSrc().ordinal();
		this.soLanNopLuu = form.getTimes() == null ? 0 : Integer.valueOf(form.getTimes());
		this.tongSoHoSo = form.getTotal();
		this.tongSoTrang = form.getTotalPage() == null ? 0 : form.getTotalPage();
		this.tongSoDungLuong = form.getSize() == null ? 0 : form.getSize();
		this.cachThucNopLuu = form.getWay() == null ? 0 : form.getWay().ordinal();
		this.thoiGianDuKienNop = DateTimeUtils.convertDateToStringPattern(form.getDate(),
				changeFormat ? DateTimeUtils.DD_MM_YYYY : DateTimeUtils.YYYY_MM_DD);
		this.diaChiLienHe = form.getAddress();
		this.ghiChu = form.getNote() == null ? "" : form.getNote();
		this.token = BussinessCommon.getToken();
		this.dinhKem = new ArrayList<>();
		this.callBack = BussinessCommon.getDomain() + "/api/hstl-form/update/" + form.getId();
		this.noiDung = form.getName();
		this.maYeuCau = form.getId() == null ? "0" : form.getId().toString();
	}
}
