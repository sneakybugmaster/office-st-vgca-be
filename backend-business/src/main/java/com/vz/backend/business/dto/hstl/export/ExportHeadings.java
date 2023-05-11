package com.vz.backend.business.dto.hstl.export;

import java.util.List;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Getter
@Setter
public class ExportHeadings extends ExportBase {
	private long totalUnlimit;
	private long totalLimit;
	private List<ContentFolders> contents;
	
	public ExportHeadings(List<ContentFolders> content) {
		super();
		this.contents = content;
		this.setTotal(content.stream().filter(i -> i.getLimitation() != null).count());
		this.totalLimit = content.stream().filter(i -> i.getLimitation() != null && i.getLimitation()).count();
		this.totalUnlimit = Math.abs(super.getTotal() - this.totalLimit);
	}
}