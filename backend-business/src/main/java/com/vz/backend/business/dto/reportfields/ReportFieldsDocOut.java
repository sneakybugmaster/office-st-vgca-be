package com.vz.backend.business.dto.reportfields;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.core.dto.LabelValueDto;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Getter
@NoArgsConstructor
public class ReportFieldsDocOut extends ReportFieldDf {
	private String numberOrSign;
	private String userEnter;
	private Date createDate;
	private Date dateIssued;
	private String docTypeName;
	private String preview;
	private String securityName;
	private String docStatus;

	@Override
	public Map<String, Object> getKVMap() {
		Map<String, Object> kvMap = new HashMap<>();
		kvMap.put("numberOrSign", LabelValueDto.set(this.numberOrSign, "Số kí hiệu"));
		kvMap.put("userEnter", LabelValueDto.set(this.userEnter, "Người soạn thảo"));
		kvMap.put("createDate", LabelValueDto.set(this.createDate, "Ngày tạo"));
		kvMap.put("dateIssued", LabelValueDto.set(this.dateIssued, "Ngày ban hành"));
		kvMap.put("docTypeName", LabelValueDto.set(this.docTypeName, "Loại văn bản"));
		kvMap.put("preview", LabelValueDto.set(this.preview, "Trích yếu"));
		kvMap.put("securityName", LabelValueDto.set(this.securityName, "Độ mật"));
		kvMap.put("docStatus", LabelValueDto.set(this.docStatus, "Trạng thái văn bản"));
		return kvMap;
	}

	public ReportFieldsDocOut(DocumentOut doc) {
		this.createDate = doc.getCreateDate();
		this.numberOrSign = doc.getNumberOrSign();
		this.preview = doc.getPreview();
		this.userEnter = doc.getUserEnter() != null ? doc.getUserEnter().getFullName() : "";
		this.dateIssued = doc.getDateIssued();
		this.docTypeName = doc.getDocType() != null ? doc.getDocType().getName() : "";
		this.docStatus = doc.getStatusName();
		this.securityName = doc.getSecurity() != null ? doc.getSecurity().getName() : "";
	}

	public ReportFieldsDocOut(DocumentOut doc, int no) {
		this(doc);
		super.setNo(no);
	}

	@Override
	public List<ReportFieldDf> cast(List<? extends Object> data) {
		List<ReportFieldDf> docOuts = new ArrayList<>();
		int no = 1;
		for (Object i : data) {
			docOuts.add(new ReportFieldsDocOut((DocumentOut) i, no));
			no++;
		}
		return docOuts;
	}
}