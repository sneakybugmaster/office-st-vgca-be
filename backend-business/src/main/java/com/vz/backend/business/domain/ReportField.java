package com.vz.backend.business.domain;

import java.util.Map;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.dto.LabelValueDto;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "REPORT_FEILD", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties({"clientId", "active", "createDate", "updateDate", "createBy", "updateBy" })
public class ReportField extends BaseModel {
	private String colName;
	private String colLabel;
	private Integer orderNumber;
	@Enumerated(EnumType.STRING)
	private DocumentTypeEnum type;

	@SuppressWarnings("unchecked")
	public void filter(ObjectNode objNode, Map<String, ? extends Object> map) {
		String key = this.colName;
		LabelValueDto<String> lv = (LabelValueDto<String>) map.get(key);
		objNode.put(key, lv == null ? "" : lv.getValue());
	}

	public ReportField(String colName, String colLabel, DocumentTypeEnum type) {
		this.colLabel = colLabel;
		this.colName = colName;
		this.type = type;
	}
}
