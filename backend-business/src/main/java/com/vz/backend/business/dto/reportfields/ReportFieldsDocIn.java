package com.vz.backend.business.dto.reportfields;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.dto.DocumentDto;
import com.vz.backend.core.dto.LabelValueDto;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class ReportFieldsDocIn extends ReportFieldDf {
	private Date createDate;
	private String numberArrival;
	private Date dateArrival;
	private Date recievedDate;
	private String numberOrSign;
	private String preview;
	private String placeSend;
	private String orgExe;
	private Date deadline;

	@Override
	public Map<String, Object> getKVMap() {
		Map<String, Object> kvMap = new HashMap<>();
		kvMap.put("createDate", LabelValueDto.set(this.getCreateDate(), "Ngày vào sổ"));
		kvMap.put("numberArrival", LabelValueDto.set(this.getNumberArrival(), "Số đến"));
		kvMap.put("dateArrival", LabelValueDto.set(this.getDateArrival(), "Ngày văn bản"));
		kvMap.put("recievedDate", LabelValueDto.set(this.getRecievedDate(), "Ngày nhận văn bản"));
		kvMap.put("numberOrSign", LabelValueDto.set(this.getNumberOrSign(), "Số của văn bản đến"));
		kvMap.put("preview", LabelValueDto.set(this.getPreview(), "Trích yếu"));
		kvMap.put("placeSend", LabelValueDto.set(this.getPlaceSend(), "Nơi gửi"));
		kvMap.put("orgExe", LabelValueDto.set(this.getOrgExe(), "Đơn vị xử lý"));
		kvMap.put("deadline", LabelValueDto.set(this.getDeadline(), "Hạn xử lý"));
		return kvMap;
	}
	
	public ReportFieldsDocIn(DocumentDto dto) {
		Documents doc = dto.getDoc();
		this.createDate = doc.getCreateDate();
		this.numberArrival = doc.getNumberArrivalStr();
		this.dateArrival = doc.getDateArrival();
		this.recievedDate = doc.getReceivedDate();
		this.numberOrSign = doc.getNumberOrSign();
		this.preview = doc.getPreview();
		this.placeSend = doc.getPlaceSend();
		this.orgExe = dto.getOrgExe();
		this.deadline = dto.getDeadline();
	}

	public ReportFieldsDocIn(DocumentDto dto, int no) {
		this(dto);
		super.setNo(no);
	}

	@Override
	public List<ReportFieldDf> cast(List<? extends Object> data) {
		List<ReportFieldDf> docIns = new ArrayList<>();
		int no = 1;
		for (Object i : data) {
			docIns.add(new ReportFieldsDocIn((DocumentDto) i, no));
			no++;
		}
		return docIns;
	}
}
