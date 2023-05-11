package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.vz.backend.business.domain.Fields;
import com.vz.backend.business.domain.Values;
import com.vz.backend.business.dto.ValueDto;
import com.vz.backend.business.repository.IAssignTaskRepository;
import com.vz.backend.business.repository.IDocumentRepository;
import com.vz.backend.business.repository.IFieldsRepository;
import com.vz.backend.business.repository.IValueRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.CategoryEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.repository.IRepository;
import com.vz.backend.core.service.BaseService;

@Service
public class ValueService extends BaseService<Values> {

	@Autowired
	IValueRepository vRepository;

	@Autowired
	IDocumentRepository docRepository;

	@Autowired
	IAssignTaskRepository taskRepository;

	@Autowired
	IFieldsRepository fRepository;

	@Autowired
	FieldService fService;

	@Override
	public IRepository<Values> getRepository() {
		return null;
	}

	public List<Values> saveValues(ValueDto[] arr) {
		BussinessCommon.validArr(arr);
		List<Values> vList = new ArrayList<>();
		for (ValueDto dto : arr) {
			validValues(dto);
			Values v = castToDto(dto);
			try {
				vList.add(vRepository.save(v));
			} catch (Exception e) {
				throw new RestExceptionHandler(Message.ERROR_SYS);
			}
		}
		return vList;
	}

	public List<Values> updateValues(ValueDto[] arr) {
		BussinessCommon.validArr(arr);
		List<Values> vList = new ArrayList<>();
		for (ValueDto dto : arr) {
			validValues(dto);
			Values vs = vRepository.findByClientIdAndCatIdAndFormIdAndFieldsId(BussinessCommon.getClientId(),
					Long.parseLong(dto.getCatId()), Long.parseLong(dto.getFormId()), Long.parseLong(dto.getFieldsId()));
			if (vs == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT_DATA);
			}

			Values v = castToDto(dto);
			v.setId(vs.getId());
			vList.add(vRepository.save(v));
		}
		return vList;
	}

	public void del(String catId, String fieldId) {
		fService.validField(fieldId, "id");
		fService.validField(catId, "catId");
		List<Values> v = vRepository.findByClientIdAndCatIdAndFieldsId(BussinessCommon.getClientId(),
				Long.parseLong(catId), Long.parseLong(fieldId));
		if (v == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT_DATA);
		}

		vRepository.deleteAll(v);
	}

	private Values castToDto(ValueDto dto) {
		Values v = new Values();
		v.setCatId(Long.parseLong(dto.getCatId()));
		v.setFormId(Long.parseLong(dto.getFormId()));
		v.setContent(dto.getContent());
		v.setFields(validFields(dto.getFieldsId()));
		return v;
	}

	private Fields validFields(String fieldId) {
		fService.validField(fieldId, "id");
		Fields f = fRepository.findByClientIdAndId(BussinessCommon.getClientId(), Long.parseLong(fieldId));
		if (f == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FIELD);
		}
		return f;
	}

	private void validValues(ValueDto dto) {
		fService.validField(dto, null);
		fService.validField(dto.getFieldsId(), "id");
		fService.validField(dto.getFormId(), "id");
		fService.validField(dto.getCatId(), "id");
		validFormId(Long.parseLong(dto.getCatId()), Long.parseLong(dto.getFormId()));
		Fields f = validFields(dto.getFieldsId());
		validContent(f.isRequired(), dto.getContent());
	}

	private void validContent(Boolean isRequired, String content) {
		if (isRequired) {
			if (content == null || content.length() == 0 || content.isEmpty()) {
				throw new RestExceptionHandler(Message.REQUIRE_FIELD);
			}
		}
	}

	private void validFormId(Long catId, Long formId) {
		Object obj = null;
		if (CategoryEnum.DOCUMENT.getValue() == catId.intValue()) {
			obj = docRepository.findByClientIdAndId(BussinessCommon.getClientId(), formId);
		}

		/*
		 * else if (CategoryEnum.ASSIGN.getValue() == catId.intValue()) { obj =
		 * taskRepository.findByClientIdAndId(BussinessCommon.getClient(), formId); }
		 */
		if (obj == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT_DATA);
		}
	}

	public List<Values> getValuesById(String catId, String formId) {
		fService.validField(formId, "id");
		fService.validField(catId, "id");
		List<Values> v = vRepository.findByClientIdAndCatIdAndFormId(BussinessCommon.getClientId(),
				Long.parseLong(catId), Long.parseLong(formId));
		if (v == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT_DATA);
		}
		return v;
	}
}
