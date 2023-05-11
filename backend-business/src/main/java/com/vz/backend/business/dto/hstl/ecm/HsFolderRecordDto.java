package com.vz.backend.business.dto.hstl.ecm;

import java.util.List;

import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.business.domain.hstl.HsFolderRecord;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.util.DateTimeUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Getter
public class HsFolderRecordDto {
	private Long deMuc;
	private String fileCode;
	private Integer fileCatalog;
	private String fileNotation;
	private String title;
	private Long maintenance;
	private Long mucLucHoSo;
	private String rights;
	private String language;
	private String startDate;
	private String endDate;
	private String inforSign;
	private String description;
	private String keyword;
	private String soLuongTo;
	private Integer pageNumber;
	private String format;
	private String totalDoc;
	private Long idFolderRecord;
	private String organld;
	private List<FolderAttachmentDto> danhSachTaiLieu;
	private String system = "OFFICE";
	private String maYeuCau;

	public HsFolderRecordDto(HsFolderRecord h, List<FolderAttachmentDto> danhSachTaiLieu) {
		HsFolder f = h.getHsFolder();
		this.danhSachTaiLieu = danhSachTaiLieu;
		this.fileCatalog = f.getFileCatalog() == null ? 0  : Integer.valueOf(f.getFileCatalog());
		this.fileNotation = BussinessCommon.toString(f.getFileNotation());
		this.title = BussinessCommon.toString(f.getTitle());
		this.maintenance = f.getMaintenance();
		this.mucLucHoSo = f.getHeadingsId();
		this.rights = BussinessCommon.toString(f.getRights());
		this.language = BussinessCommon.toString(f.getLanguage());
		this.startDate = DateTimeUtils.convertDateToStringPattern(f.getStartDate(), DateTimeUtils.YYYY_MM_DD);
		this.endDate = DateTimeUtils.convertDateToStringPattern(f.getEndDate(), DateTimeUtils.YYYY_MM_DD);
		this.inforSign = BussinessCommon.toString(f.getInforSign());
		this.description = BussinessCommon.toString(f.getDescription());
		this.keyword = BussinessCommon.toString(f.getKeyword());
		this.soLuongTo = f.getSheerNumber() == 0 ? "0" : String.valueOf(f.getSheerNumber());
		this.pageNumber = f.getPageNumber();
		this.format = BussinessCommon.toString(f.getFormat());
		this.fileCode = BussinessCommon.toString(f.getFileCode());
		this.totalDoc = f.getTotalDoc() == null ? "0" : String.valueOf(f.getTotalDoc());
		this.deMuc = f.getHeadingsId();
		this.idFolderRecord = h.getId();
		this.organld = BussinessCommon.toString(f.getOrganld());
		this.maYeuCau = h.getFormId() == null ? "0" : h.getFormId().toString();
	}
}
