package com.vz.backend.business.dto.hstl;

import java.util.ArrayList;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.domain.hstl.Headings;
import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.core.config.Constant;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@NoArgsConstructor
@Getter
public class RecordHeadings {
	@Setter
	private String fileNotation;
	private String title;
	private String maintenance;
	private String creator;
	private String description;
	@JsonIgnore
	private Boolean limitation;

	public RecordHeadings(String headings, HsFolder f) {
		this.fileNotation = f != null ? convert(f.getFileNotation()) : "";
		this.title = f != null ? convert(f.getTitle()) : headings;
		this.maintenance = f != null ? convert(f.getMainternanceStr()) : "";
		this.creator = f != null ? convert(f.getCreator()) : "";
		this.description = f != null ? convert(f.getDescription()) : "";
		this.limitation = f != null ? !Constant.THVV.equals(f.getMaintenanceObj().getCode()) : null;
	}

	private String convert(String s) {
		return s == null ? "" : s;
	}

	public static List<RecordHeadings> convert(List<Headings> headings, List<HsFolder> folders) {
		List<RecordHeadings> rs = new ArrayList<>();
		RecordHeadings tmp = null;
		for (Headings i : headings) {
			tmp = new RecordHeadings(i.getName(), null);
			rs.add(tmp);
			for (HsFolder f : folders) {
				if (i.getId().equals(f.getHeadingsId())) {
					tmp = new RecordHeadings(i.getName(), f);
					rs.add(tmp);
				}
			}
		}

		setNumber(rs);
		return rs;
	}

	private static void setNumber(List<RecordHeadings> rs) {
		int no = 1;
		for (int i = 0; i < rs.size(); i++) {
			RecordHeadings r = rs.get(i);
			r.setFileNotation(no + ". " + r.getFileNotation());
			no++;
		}
	}
}
