package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.vz.backend.business.domain.ReportField;
import com.vz.backend.business.dto.reportfields.ReportFieldDf;
import com.vz.backend.business.dto.reportfields.ReportFieldsDocIn;
import com.vz.backend.business.dto.reportfields.ReportFieldsDocOut;
import com.vz.backend.business.dto.reportfields.ReportFieldsTask;
import com.vz.backend.business.repository.IReportFieldRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.dto.LabelValueDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ReportFieldService extends BaseService<ReportField> {

	@Autowired
	IReportFieldRepository reportFieldRepository;

	@Override
	public IRepository<ReportField> getRepository() {
		return reportFieldRepository;
	}
	
	/**
	 * Define object type 
	 * @param type 
	 * @return
	 */
	private ReportFieldDf getRFObj(DocumentTypeEnum type) {
		ReportFieldDf rpObj = null;
		switch (type) {
		case VAN_BAN_DEN:
			rpObj = new ReportFieldsDocIn();
			break;
		case VAN_BAN_DI:
			rpObj = new ReportFieldsDocOut();
			break;
		case GIAO_VIEC:
			rpObj = new ReportFieldsTask();
			break;
		default:
			log.info("Not found type object");
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}
		return rpObj;
	}

	/**
	 * Save report fields by object type in DB
	 * Report fields define in sub-object extends ReportFieldDf
	 * @param type
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public List<ReportField> saveByObjType(DocumentTypeEnum type) {
		delByObjType(type);
		List<ReportField> rpFields = new ArrayList<>();
		ReportFieldDf rpObj = getRFObj(type);
		Map<String, ? extends Object> kvMap = rpObj.getKVMap();
		kvMap.forEach((k, v) -> rpFields.add(new ReportField(k, ((LabelValueDto<String>) v).getLabel(), type)));
		return reportFieldRepository.saveAll(rpFields);
	}

	private void delByObjType(DocumentTypeEnum type) {
		List<ReportField> oldReportFields = list(type);
		if (!oldReportFields.isEmpty()) {
			reportFieldRepository.deleteAll(oldReportFields);
		}
	}

	public List<ReportField> list(DocumentTypeEnum type) {
		return reportFieldRepository.findByClientIdAndTypeAndActiveTrueOrderByOrderNumberDesc(BussinessCommon.getClientId(), type);
	}

	/**
	 * Filtered out redundancy of data
	 * @param type
	 * @param objData
	 * @return
	 */
	private List<ReportFieldDf> getDataFiltered(DocumentTypeEnum type, List<? extends Object> objData) {
		ReportFieldDf rpObj = getRFObj(type);
		return rpObj.cast(objData);
	}

	@SuppressWarnings("deprecation")
	public ObjectNode getData(DocumentTypeEnum type, List<? extends Object> objData) {
		List<ReportFieldDf> data = getDataFiltered(type, objData);
		List<ReportField> rpFields = list(type).stream()
				.filter(i -> i.getOrderNumber() != null && i.getOrderNumber() > 0).collect(Collectors.toList());
		ObjectMapper mapper = new ObjectMapper();
		ObjectNode rs = mapper.createObjectNode();
		ObjectNode colName = mapper.createObjectNode();
		ArrayNode colData = mapping(rpFields, data);
		colName.put("no", "Số thứ tự");
		rpFields.forEach(i -> colName.put(i.getColName(), i.getColLabel()));
		rs.put("colName", colName);
		rs.put("colData", colData);
		return rs;
	}

	/**
	 * Mapping report fields and column of data is getting from DB together
	 * @param rpFields
	 * @param data
	 * @return
	 */
	private ArrayNode mapping(List<ReportField> rpFields, List<ReportFieldDf> data) {
		ObjectMapper mapper = new ObjectMapper();
		ArrayNode arr = mapper.createArrayNode();
		for (ReportFieldDf i : data) {
			ObjectNode obj = mapper.createObjectNode();
			obj.put("no", i.getNo());
			rpFields.forEach(j -> j.filter(obj, i.getKVMap()));
			arr.add(obj);
		}
		return arr;
	}

	
	public List<ReportField> updateOrderNumber(DocumentTypeEnum type, List<ReportField> nReportFields) {
		Map<Long, Integer> temp = new HashMap<>();
		nReportFields.forEach(i -> {
			Long key = i.getId();
			if (key != null && !temp.containsKey(key)) {
				temp.put(key, i.getOrderNumber());
			}
		});

		List<ReportField> oReportFields = list(type);
		for (ReportField i : oReportFields) {
			Long key = i.getId();
			if (temp.containsKey(key)) {
				i.setOrderNumber(temp.get(key));
				reportFieldRepository.save(i);
			}
		}
		return oReportFields;
	}
}
