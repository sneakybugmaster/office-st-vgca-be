package com.vz.backend.business.dto.document;

import java.util.ArrayList;
import java.util.List;

import com.vz.backend.business.domain.Documents;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.service.OrganizationService;
import com.vz.backend.util.DateTimeUtils;

import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;

@Data
@NoArgsConstructor
public class ResolvedDocumentDto {

	private String numberArrival;
	private String dateArrival;
	private String numberOrSign;
	private String preview;
	private String placeSendName;
	private List<ResolvedUserDto> contentUnits = new ArrayList<>();
	private List<ResolvedUserDto> contents = new ArrayList<>();
	private String orgName;
	private String orgNameUpper;
	private String position;
	private String singer;

	private String singerUpper;

	private String ngayVB;
	private String thangVB;
	private String namVB;

	public ResolvedDocumentDto(Documents doc) {
		this.numberArrival = convert(doc.getNumberArrival());
		this.dateArrival = DateTimeUtils.dateToString(doc.getDateArrival());
		this.numberOrSign = convert(doc.getNumberOrSign());
		this.preview = doc.getPreview();
		this.placeSendName = doc.getPlaceSends() != null ? doc.getPlaceSends().getName() : "";
		this.ngayVB = DateTimeUtils.getStringDay(doc.getDateIssued());
		this.thangVB = DateTimeUtils.getStringMonth(doc.getDateIssued());
		this.namVB = DateTimeUtils.getStringYear(doc.getDateIssued());
	}

	private <T> String convert(T obj) {
		return obj != null ? obj.toString() : "";
	}

	public ResolvedDocumentDto(Documents doc, List<ResolvedUserDto> datas) {
		this(doc);
		for (ResolvedUserDto i : datas) {
			if (i.isLeaderShip()) {
				this.contents.add(i);
			} else {
				this.contentUnits.add(i);
			}
		}
	}
}
