package com.vz.backend.business.service;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Stream;

import javax.persistence.EntityManagerFactory;

import org.apache.commons.lang.ArrayUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.google.common.collect.Iterables;
import com.vz.backend.business.domain.Fields;
import com.vz.backend.business.dto.FieldDto;
import com.vz.backend.business.dto.FieldOptionDto;
import com.vz.backend.business.dto.ObjectFieldDto;
import com.vz.backend.business.repository.IFieldsRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.CategoryEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DataTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.StringUtils;

@Service
public class FieldService {
	@Autowired
	IFieldsRepository fRepository;

	@Autowired
	EntityManagerFactory entityManagerFactory;

	public List<Fields> save(ObjectFieldDto<FieldDto> obj) {
		FieldDto[] arr = obj.getObjects();
		BussinessCommon.validArr(arr);
		List<Fields> fList = new ArrayList<>();
		for (FieldDto dto : arr) {
			validFields(dto, Constant.SAVE);
			Fields f = dtoCastToFields(dto);
			fRepository.save(f);
			fList.add(f);
		}
		return fList;
	}

	public List<Fields> update(ObjectFieldDto<FieldDto> obj) {
		FieldDto[] arr = obj.getObjects();
		BussinessCommon.validArr(arr);
		List<Fields> fList = new ArrayList<>();
		for (FieldDto dto : arr) {
			validFields(dto, Constant.UPDATE);
			validField(dto.getId(), "id");

			Long id = Long.parseLong(dto.getId());
			if (!isExistId(id)) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FIELD);
			}
			Fields f = dtoCastToFields(dto);
			f.setId(id);

			fRepository.save(f);
			fList.add(f);
		}
		return fList;
	}

	public void del(ObjectFieldDto<FieldDto> obj) {
		FieldDto[] arr = obj.getObjects();
		BussinessCommon.validArr(arr);
		for (FieldDto dto : arr) {
			validField(dto.getId(), "id");
			Long id = Long.parseLong(dto.getId());
			if (!isExistId(id)) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FIELD);
			}
			fRepository.deleteById(id);
		}
	}

	private void validFields(FieldDto f, String action) {
		validField(f, null);
		validField(f.getCatId(), "catId");
		validField(f.getType(), "type");
		validFOption(f.getId(), f.getType(), f.getFieldOption(), action);
		validField(f.getName(), "name");
		validField(f.getLabel(), null);
		validName(f.getId(), f.getName(), Long.parseLong(f.getCatId()), action);
	}

	private void validName(String id, String newName, Long catId, String action) {
		if (Constant.SAVE.equals(action) && isExistName(newName, catId)) {
			throw new RestExceptionHandler(Message.EXIST_FIELD);
		} else if (Constant.UPDATE.equals(action)) {
			validField(id, "id");
			Fields f = fRepository.findByClientIdAndId(BussinessCommon.getClientId(), Long.parseLong(id));
			if (!f.getName().equals(newName) && isExistName(newName, catId)) {
				throw new RestExceptionHandler(Message.EXIST_FIELD);
			}
		}
	}

	private void validFOption(String id, String type, FieldOptionDto[] arr, String action) {
		if (Constant.UPDATE.equals(action)) {
			validField(id, "id");
			Fields f = fRepository.findByClientIdAndId(BussinessCommon.getClientId(), Long.parseLong(id));
			if (!type.equals(f.getType())) {
				throw new RestExceptionHandler(Message.INVALID_UPDATE_TYPE_DATA);
			}
		}

		if (DataTypeEnum.RADIO.getValue().equals(type) || DataTypeEnum.CHECKBOX.getValue().equals(type)
				|| DataTypeEnum.AUTOCOMPLETE.getValue().equals(type)) {
			if (ArrayUtils.isEmpty(arr) || arr == null || arr.length == 0) {
				throw new RestExceptionHandler(Message.INVALID_OPTION_FIELD);
			}
		} else {
			if (arr != null && arr.length > 0) {
				throw new RestExceptionHandler(Message.INVALID_OPTION_FIELD);
			}
		}
	}

	public boolean isExistId(Long id) {
		return fRepository.findByClientIdAndId(BussinessCommon.getClientId(), id) != null;
	}

	private boolean isExistName(String name, Long catId) {
		return getFieldByObject(name, catId) != null;
	}

	private Fields getFieldByObject(String name, Long catId) {
		validField(catId, "catId");
		validField(name, "name");
		Fields f = fRepository.getFieldByNameAndCatIdAndClientId(name, catId, BussinessCommon.getClientId());
		return f != null ? f : null;
	}

	/***
	 * validate input by type check
	 *
	 * @param <T>
	 * @param input
	 * @param type
	 * @return
	 */
	public <T> String validField(T input, String type) {
		if (input == null || input.toString().length() == 0) {
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
		}

		if ("type".equals(type)) {
			if (DataTypeEnum.getEnum(input.toString()) == null) {
				throw new RestExceptionHandler(Message.TYPE_DATA_FIELD);
			}
		}

		if ("catId".equals(type)) {
			if (CategoryEnum.getEnum(Long.parseLong(input.toString())) == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT);
			}
		}

		if ("name".equals(type)) {
			if (input.toString().length() > Constant.MAX_LENGTH_NAME) {
				throw new RestExceptionHandler(Message.MAX_LENGTH_FIELD);
			}
		}

		if ("id".equals(type)) {
			if (!StringUtils.isInteger(input.toString())) {
				throw new RestExceptionHandler(Message.INVALID_ID);
			}
		}
		return input.toString();
	}

	public void validType(String type, String content) {
		if (DataTypeEnum.NUMBER.getValue().equals(type) && content != null && content.length() > 0) {
			if (!StringUtils.isInteger(content)) {
				throw new RestExceptionHandler(Message.INVALID_NUMBER_FIELD);
			}
		}

		if (DataTypeEnum.DATE.getValue().equals(type) && content != null && content.length() > 0) {
			if (!StringUtils.isValidDate(content)) {
				throw new RestExceptionHandler(Message.INVALID_DATE_FIELD);
			}
		}

		// validate File
	}

	/**
	 * cast FieldOption To Array
	 *
	 * @param input
	 * @return
	 */
	private FieldOptionDto[] castFieldOptionToArr(String input) {
		List<FieldOptionDto> list = new ArrayList<>();
		try {
			String[] listFieldOption = input.split(",");
			String[] sub = null;
			FieldOptionDto f = null;
			for (String fieldOption : listFieldOption) {
				sub = fieldOption.split("-");
				f = new FieldOptionDto();
				f.setValue(sub[0]);
				f.setLabel(sub[1]);
				list.add(f);
			}
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.INVALID_OPTION_FIELD);
		}
		return Iterables.toArray(list, FieldOptionDto.class);
	}

	/**
	 * cast FieldOption To String
	 *
	 * @param input
	 * @return
	 */
	private String castFieldOptionToStr(FieldOptionDto[] input) {
		StringBuffer str = new StringBuffer("");
		if (isDuplicateArr(input)) {
			throw new RestExceptionHandler(Message.INVALID_LABEL);
		}
		try {
			for (FieldOptionDto f : input) {
				str.append(f.getValue() + "-" + f.getLabel());
				if (input != null) {
					str.append(",");
				}
			}
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.INVALID_OPTION_FIELD);
		}
		return str.toString();
	}

	private boolean isDuplicateArr(FieldOptionDto[] arr) {
		String[] str = new String[arr.length];
		for (int i = 0; i < arr.length; i++) {
			str[i] = arr[i].getLabel();
		}

		Long distinctCount = Stream.of(str).distinct().count();
		return str.length != distinctCount;
	}

	public List<FieldDto> getListByClientIdAndCatId(String catId) {
		validField(catId, "catId");
		List<Fields> fList = fRepository.findByClientIdAndCatId(BussinessCommon.getClientId(), Long.parseLong(catId));
		List<FieldDto> dtoList = new ArrayList<>();
		for (Fields f : fList) {
			dtoList.add(fieldsCastToDto(f));
		}
		return dtoList;
	}

	private Fields dtoCastToFields(FieldDto dto) {
		Fields f = new Fields();
		f.setCatId(Long.parseLong(dto.getCatId()));
		f.setFieldOption(castFieldOptionToStr(dto.getFieldOption()));
		f.setType(dto.getType());
		f.setName(dto.getName());
		f.setPlaceholder(dto.getPlaceholder());
		f.setRequired(dto.isRequired());
		f.setLabel(dto.getLabel());
		return f;
	}

	private FieldDto fieldsCastToDto(Fields f) {
		FieldDto dto = new FieldDto();
		dto.setId(String.valueOf(f.getId()));
		dto.setCatId(String.valueOf(f.getCatId()));
		dto.setFieldOption(
				f.getFieldOption() != null && f.getFieldOption().length() > 0 ? castFieldOptionToArr(f.getFieldOption())
						: null);
		dto.setType(f.getType());
		dto.setName(f.getName());
		dto.setPlaceholder(f.getPlaceholder());
		dto.setRequired(f.isRequired());
		dto.setLabel(f.getLabel());
		return dto;
	}
}
