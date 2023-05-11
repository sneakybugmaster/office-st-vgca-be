package com.vz.backend.business.dto.hstl.export;

import java.util.List;

import com.vz.backend.util.NumToVietnamese;

import lombok.Getter;
import lombok.NoArgsConstructor;

@NoArgsConstructor
@Getter
public class ExportFolders extends ExportBase {
	private String maintenanceType;
	private String totalStr;
	private List<ContentFolders> contents;

	/**
	 * 
	 * @param content
	 * @param unlimit
	 * @param orgName: lấy tổ chức cấp cục
	 */
	public ExportFolders(List<ContentFolders> content, boolean unlimit, String orgName) {
		super();
		this.contents = content;
		this.setTotal(content.stream().filter(i -> i.getLimitation() != null).count());
		this.totalStr = NumToVietnamese.num2String(this.getTotal());
		this.maintenanceType = unlimit ? "Bảo quản vĩnh viễn " : "Bảo quản có thời hạn";
		this.setOrgNameUpper(orgName.toUpperCase());
		this.setOrgNameLower(orgName);
	}
}
