package com.vz.backend.core.common;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang.ArrayUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.jpa.domain.JpaSort;
import org.springframework.lang.NonNull;
import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.vz.backend.core.auth.SecurityContext;
import com.vz.backend.core.config.ActionEnum;
import com.vz.backend.core.config.AuthenticationProvider;
import com.vz.backend.core.config.CategoryEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.dto.ListObjectDtoOrder;
import com.vz.backend.core.dto.SearchDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestFieldExceptionHandler;
import com.vz.backend.core.exception.RestResponse;
import com.vz.backend.util.StringUtils;

public class BussinessCommon {

	private static final String REGEX_EMAIL = "^[A-Za-z0-9+_.-]+@[A-Za-z0-9.-]+$";
	private static final String REGEX_ID_CODE_ORG = "^\\d{3}(.)\\d{2}(.)\\d{2}(.)\\w{3}$";
	private static final String REGEX_FILE_CODE = "^\\d{3}(.)\\d{2}(.)\\d{2}(.)\\w{3}(.)\\d{4}(.)\\d{2}(.)\\w*$";
	private static final String REGEX_FILE_NOTATION = "^\\d{2}(.)\\w*$";

	private static final String DIVISION = "/";
	public static final String SEPARATOR = ",";
	
	public static <T> Page<T> getListToPage(List<T> list, Integer pageNumber) {
		Pageable pageable = BussinessCommon.castToPageable(pageNumber);
		if (BussinessCommon.isEmptyList(list))
			new PageImpl<>(new ArrayList<>(), pageable, 0);

		int total = list.size();
		int rowCount = Constant.NUMBER_OF_PAGE;
		int start = pageNumber.intValue() > 1 ? (pageNumber - 1) * rowCount : 0;
		int end = Math.min((start + rowCount), total);
		List<T> output = new ArrayList<>();
		if (start <= end) {
			output = list.subList(start, end);
		}
		return new PageImpl<>(output, pageable, total);
	}
	
	public static <T> Page<T> getListToPage(List<T> list, Pageable pageable) {
		if (BussinessCommon.isEmptyList(list))
			new PageImpl<>(new ArrayList<>(), pageable, 0);

		Integer pageNumber = pageable.getPageNumber()+1;
		int size = pageable.getPageSize();
		
		int total = list.size();
		int rowCount = size;
		int start = pageNumber.intValue() > 1 ? (pageNumber - 1) * rowCount : 0;
		int end = Math.min((start + rowCount), total);
		List<T> output = new ArrayList<>();
		if (start <= end) {
			output = list.subList(start, end);
		}
		return new PageImpl<>(output, pageable, total);
	}
	
	public static <T> ListObjectDto<T> getListByPageNumber(List<T> list, Integer pageNumber) {
		ListObjectDto<T> objectList = new ListObjectDto<>();
		if (list != null && !list.isEmpty()) {
			long totalRecords = list.size();
			int rowCount = Constant.NUMBER_OF_PAGE;
			long totalPage = totalRecords % rowCount > 0 ? totalRecords / rowCount + 1 : totalRecords / rowCount;
			if (pageNumber != null && pageNumber.intValue() == 0
					|| pageNumber != null && pageNumber.intValue() > totalPage) {
				throw new RestExceptionHandler(Message.INVALID_PAGE);
			}
			int offset = pageNumber != null && pageNumber.intValue() > 1 ? (pageNumber - 1) * rowCount : 0;
			objectList.setObjList(list.subList(offset, (int) Math.min(offset + rowCount, totalRecords)));
			objectList.setTotalPage(totalPage);
			objectList.setTotalRecord(totalRecords);
		}
		return objectList;
	}

	public static <T> ListObjectDto<T> paging(Page<T> pList) {
		ListObjectDto<T> list = new ListObjectDto<>();
		if (pList == null || pList.isEmpty()) {
			return list;
		}
		list.setObjList(pList.getContent());
		list.setTotalPage(pList.getTotalPages());
		list.setTotalRecord(pList.getTotalElements());
		return list;
	}

	public static <T> ListObjectDto<T> convert(List<T> pList) {
		ListObjectDto<T> list = new ListObjectDto<>();
		if (pList == null || pList.isEmpty()) {
			return list;
		}
		long totalRecords = pList.size();
		int rowCount = Constant.NUMBER_OF_PAGE;
		long totalPage = totalRecords % rowCount > 0 ? totalRecords / rowCount + 1 : totalRecords / rowCount;
		list.setObjList(pList);
		list.setTotalPage(totalPage);
		list.setTotalRecord(totalRecords);
		return list;
	}

	public static <T> ListObjectDtoOrder<T> paging(Page<T> pList, long order) {
		ListObjectDtoOrder<T> list = new ListObjectDtoOrder<>();
		list.setNextOrder(order);
		if (pList == null || pList.isEmpty()) {
			return list;
		}
		list.setTotalPage(pList.getTotalPages());
		list.setObjList(pList.getContent());
		list.setTotalRecord(pList.getTotalElements());
		// list.setNextOrder(order);
		return list;
	}

	public static Integer setPageNumber(String page) {
		if ("".equals(page) || page == null || page.length() == 0) {
			return 1;
		}

		if (Integer.parseInt(page) <= 0 || !StringUtils.isInteger(page)) {
			throw new RestExceptionHandler(Message.INVALID_PAGE);
		}

		return Integer.parseInt(page);
	}

	public static Long castId(String id) {
		if ("".equals(id) || id == null || id.length() == 0) {
			return null;
		}

		if (Integer.parseInt(id) <= 0 || !StringUtils.isInteger(id)) {
			throw new RestExceptionHandler(Message.INVALID_ID);
		}

		return Long.parseLong(id);
	}

	public static void validId(String id, boolean skipError) {
		if (StringUtils.isNullOrEmpty(id)) {
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
		}

		try {
			long tmp = Long.parseLong(id);
			if(tmp <= 0 && !skipError) {
				throw new RestExceptionHandler(Message.INVALID_ID);
			}
		} catch (Exception e) {
			e.printStackTrace();
			if (!skipError) {
				throw new RestExceptionHandler(Message.INVALID_ID);
			}
		}
	}

	public static String castString(String name) {
		if ("".equals(name) || name == null || name.length() == 0) {
			return null;
		}
		return name;
	}
	
	public static Pageable castToPageable(Integer page, int size) {
		try {
			return PageRequest.of(page - 1, size);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.INVALID_PAGE);
		}
	}

	public static Pageable castToPageable(Integer page) {
		try {
			return PageRequest.of(page - 1, Constant.NUMBER_OF_PAGE);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.INVALID_PAGE);
		}
	}

	public static Pageable castToPageable(Integer page, Sort sort) {
		try {
			return PageRequest.of(page - 1, Constant.NUMBER_OF_PAGE, sort);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.INVALID_PAGE);
		}
	}

	public static Pageable castToPageable(Integer page, Sort sort, int numberOfPage) {
		try {
			return PageRequest.of(page - 1, numberOfPage, sort);
		} catch (Exception e) {
			throw new RestExceptionHandler(Message.INVALID_PAGE);
		}
	}

	@NonNull
	public static Long getClientId() {
		User user = getUser();
		Long clientId = user.getClientId();
		if (clientId == null) {
			throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
		}
		return clientId;
	}

	@NonNull
	public static Long getOrgId() {
		Long orgId = getUser().getOrg();
		if (orgId == null) {
			throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
		}
		return orgId;
	}

	@NonNull
	public static Long getUserId() {
		User user = getUser();
		Long id = user.getId();
		if (id == null) {
			throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
		}
		return id;
	}

	@NonNull
	public static User getUser() {
		User u = SecurityContext.getCurrentUser();
		if (u == null) {
			throw new RestExceptionHandler(Message.INVALID_ACCOUNT);
		}
		return u;
	}

	public static <T> void validArr(T[] arr) {
		if (ArrayUtils.isEmpty(arr) || arr == null || arr.length == 0) {
			throw new RestExceptionHandler(Message.INVALID_FIELD);
		}
	}

	public static HandleTypeEnum setTypeEnum(HandleTypeEnum type) {
		if (HandleTypeEnum.MAIN.equals(type)) {
			return HandleTypeEnum.MAIN;
		} else if (HandleTypeEnum.SUPPORT.equals(type)) {
			return HandleTypeEnum.SUPPORT;
		} else {
			return HandleTypeEnum.SHOW;
		}
	}

	public static String getTypeEnum(HandleTypeEnum type, Boolean direction) {
		String typeName;
		switch (type) {
		case MAIN:
			typeName = Boolean.TRUE.equals(direction) ? Constant.DIRECTION : Constant.MAIN;
			break;
		case SUPPORT:
			typeName = Constant.SUPPORT;
			break;
		case DIRECTION:
			typeName = Constant.DIRECTION;
			break;
		case SHOW:
			typeName = Constant.SHOW;
			break;
		default:
			typeName = Constant.SHOW;
			break;
		}

		return typeName;
	}

	public static String getAction(String type) {
		if (ActionEnum.ADD.getName().equals(type)) {
			return Constant.ACTION_ADD;
		} else if (ActionEnum.TRANSFER_HANDLE.getName().equals(type)) {
			return Constant.ACTION_TRANFER_HANDLE;
		} else if (ActionEnum.COMMING_TO.getName().equals(type)) {
			return Constant.ACTION_COMMING_TO;
		} else if (ActionEnum.READ.getName().equals(type)) {
			return Constant.ACTION_READ;
		} else if (ActionEnum.RETURN_DOC.getName().equals(type)) {
			return Constant.ACTION_RETURN_DOC;
		} else if (ActionEnum.ISSUED.getName().equals(type)) {
			return Constant.ACTION_ISSUED;
		} else if (ActionEnum.COMMENT.getName().equals(type)) {
			return Constant.ACTION_COMMENT;
		} else if (ActionEnum.LOGIN.getName().equals(type)) {
			return Constant.ACTION_LOGIN;
		} else if (ActionEnum.DONE.getName().equals(type)) {
			return Constant.ACTION_DONE;
		} else {
			return Constant.ACTION_UNKNOW;
		}
	}

	public static String getAction(DocumentInHandleStatusEnum status, boolean readToDoing) {
		String st = "";
		switch (status) {
		case DANG_XU_LY:
			st = readToDoing ? "Đang xử lý" : "Đã xem";
			break;
		case CHO_XU_LY:
			st = "Mới đến";
			break;
		default:
			st = status.getName();
			break;
		}
		return st;
	}
	
	public static String getAction(DocumentInHandleStatusEnum status, boolean readToDoing,
			DocumentStatusEnum docStatus) {
		if (DocumentInHandleStatusEnum.DA_XU_LY.equals(status) && DocumentStatusEnum.DONE.equals(docStatus)) {
			return "Hoàn thành";
		}
		return getAction(status, readToDoing);
	}

	public static void validCatId(Long catId) {
		if (CategoryEnum.getEnum(catId) == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT);
		}
	}

	public static Long[] castStringToLongArray(String input) {
		List<Long> idList = new ArrayList<>();
		Long[] idLs = new Long[0];
		String tempReplyIds = StringUtils.cutAllSpace(input);
		if (!StringUtils.isNullOrEmpty(input, true) && tempReplyIds != null) {
			String[] ids = tempReplyIds.split(",");
			if (!ArrayUtils.isEmpty(ids)) {
				for (String st : ids) {
					try {
						BussinessCommon.validId(st, false);
						idList.add(Long.valueOf(st));
					} catch (Exception e) {
						e.printStackTrace();
					}
				}
				idLs = idList.stream().toArray(n -> new Long[n]);
			}
		}
		return idLs;
	}
	
	public static List<Long> stringToList(String input) {
		List<Long> idList = new ArrayList<>();
		if (StringUtils.isNullOrEmpty(input))
			return idList;
		
		String[] ids = StringUtils.cutAllSpace(input).split(",");
		if (!ArrayUtils.isEmpty(ids)) {
			for (String st : ids) {
				try {
					BussinessCommon.validId(st, true);
					idList.add(Long.valueOf(st));
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
		return idList;
	}

	public static <T> boolean isEmptyArr(T[] arr) {
		if (ArrayUtils.isEmpty(arr) || arr == null || arr.length == 0) {
			return true;
		}
		return false;
	}

	public static <T> boolean isEmptyPage(Page<T> page) {
		if (page != null && page.getTotalElements() > 0) {
			return false;
		}
		return true;
	}

	public static <T> boolean isEmptyList(List<T> list) {
		if (list == null || list.isEmpty())
			return true;
		for (T t : list) {
			if (t == null)
				return true;
		}
		return false;
	}

	public static String convert(String str) {
		return StringUtils.isNullOrEmpty(str) ? null : str.trim().toLowerCase();
	}

	public static Long convert(Long str) {
		return str == null || str.longValue() == 0L ? null : str;
	}

	public static <T> T convert(T obj) {
		if (obj instanceof Long) {
			return obj.equals(0L) ? null : obj;
		} else if(obj instanceof Integer) {
			return obj.equals(0L) ? null : obj;
		} else if(obj instanceof Float) {
			return obj.equals(0L) ? null : obj;
		} else {
			return obj == null ? null : obj;
		}
	}

	public static String cutCharacter(String str, int num, boolean isRequire, String field, String fieldName) {
		if (isRequire && StringUtils.isNullOrEmpty(str)) {
			throw new RestFieldExceptionHandler(field, fieldName + Message.NO_INPUT);
		}
		if (field.equals("phone") && !org.apache.commons.lang.StringUtils.isNumericSpace(str)) {
			throw new RestFieldExceptionHandler(field, fieldName + Message.FIELD_INVALID);
		}
		if (!StringUtils.isNullOrEmpty(str)) {
			if (length(str) > num) {
				return str.trim().substring(0, num);
			}
			if (length(str) <= num) {
				return str.trim();
			}
		}
		return str != null ? str.trim() : "";
	}

	public static int length(String str) {
		return str != null ? str.trim().length() : 0;
	}

	public static Long required(Long f, String field, String fieldName) {
		if (f == null || f == 0L) {
			throw new RestFieldExceptionHandler(field, fieldName + Message.NO_INPUT);
		}
		return f;
	}

	public static Date getZeroTimeDate(Date date) {
		Date res;
		Calendar calendar = Calendar.getInstance();

		calendar.setTime(date);
		calendar.set(Calendar.HOUR_OF_DAY, 0);
		calendar.set(Calendar.MINUTE, 0);
		calendar.set(Calendar.SECOND, 0);
		calendar.set(Calendar.MILLISECOND, 0);

		res = calendar.getTime();

		return res;
	}

	// type = 1 : get first / type = 2: get last
	public static Long convert(String str, int type) {
		if ("".equals(str) || str == null || str.length() == 0) {
			return null;
		}
		String[] arr = str.split("-");
		for (String element : arr) {
			if (!isNumeric(element)) {
				return null;
			}
		}
		if (type == 1 && arr.length >= 1) {
			return Long.parseLong(arr[0]);
		}
		if (type == 2 && arr.length >= 2) {
			return Long.parseLong(arr[1]);
		}
		return null;
	}

	public static boolean isNumeric(String str) {
		try {
			Long.parseLong(str);
			return true;
		} catch (NumberFormatException e) {
			return false;
		}
	}

	public static void validLengthData(String str, String column, int max) {
		if (!StringUtils.isNullOrEmpty(str) && str.length() > max) {
			throw new RestExceptionHandler(column + " chỉ tối đa " + max + " kí tự.");
		}
	}
	
	public static String castToAcronym(String str) {
		return str.replaceAll("\\B.|\\P{L}", "").toUpperCase();
	}
	
	public static boolean isEmail(String email) {
		try {
			Pattern pattern = Pattern.compile(REGEX_EMAIL);
			Matcher matcher = pattern.matcher(email);
			return matcher.matches();
		} catch (Exception e) {
			return false;
		}
	}
	//REGEX_ID_CODE_ORG
	public static boolean isIdCode(String code) {
		try {
			Pattern pattern = Pattern.compile(REGEX_ID_CODE_ORG);
			Matcher matcher = pattern.matcher(code);
			return matcher.matches();
		} catch (Exception e) {
			return false;
		}
	}
	
	public static boolean isFileCode(String code) {
		try {
			Pattern pattern = Pattern.compile(REGEX_FILE_CODE);
			Matcher matcher = pattern.matcher(code);
			return matcher.matches();
		} catch (Exception e) {
			return false;
		}
	}
	
	public static boolean isFileNotation(String code) {
		try {
			Pattern pattern = Pattern.compile(REGEX_FILE_NOTATION);
			Matcher matcher = pattern.matcher(code);
			return matcher.matches();
		} catch (Exception e) {
			return false;
		}
	}
	
	public static String part(String text, int num) {
		if (StringUtils.isNullOrEmpty(text) || !text.contains(DIVISION))
			return "";
		int division = text.lastIndexOf(DIVISION);
		if (num == 1)
			return text.substring(0, division);
		return text.substring(division + 1);
	}
	
	public static <T> void require(String field, T value) {
		if (value == null || StringUtils.isNullOrEmpty(value.toString()))
			throw new RestExceptionHandler(field + " bắt buộc phải nhập");
	}

	public static Pageable toPage(SearchDto dto) {
		Sort sort = JpaSort.unsafe(dto.getDirection(), dto.getSortBy().field);
		return BussinessCommon.castToPageable(dto.getPage(), sort, dto.getSize());
	}
	
	public static <T> String joinOrStar(List<T> list, String symbol) {
		if (list == null || list.isEmpty()) {
			return symbol;
		}
		StringBuilder result = new StringBuilder();
		result.append(SEPARATOR);
		list.forEach(e -> {
			result.append(e.toString());
			result.append(SEPARATOR);
		});
		return result.toString();
	}
	
	public static List<Long> toListLong(String str) {
		if (str == null || "-".equals(str)) {
			return new ArrayList<>();
		}
		return StringUtils.parse(str, Long::parseLong);
	}
	
	public static List<Integer> toListInteger(String str) {
		if (str == null || "-".equals(str)) {
			return new ArrayList<>();
		}
		return StringUtils.parse(str, Integer::parseInt);
	}
	
	public static String integerToRoman(int num) {
		int[] values = { 1000, 900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1 };
		String[] romanLiterals = { "M", "CM", "D", "CD", "C", "XC", "L", "XL", "X", "IX", "V", "IV", "I" };

		StringBuilder roman = new StringBuilder();

		for (int i = 0; i < values.length; i++) {
			while (num >= values[i]) {
				num -= values[i];
				roman.append(romanLiterals[i]);
			}
		}
		
		return roman.toString();
	}
	
	public static <T>  String toString(T obj) {
		return obj == null ? "" : obj.toString();
	}
	
	public static HttpServletRequest getCurrentRequest() {
		RequestAttributes requestAttributes = RequestContextHolder.getRequestAttributes();
		HttpServletRequest request = ((ServletRequestAttributes) requestAttributes).getRequest();
		return request;
	}

	/**
	 * To string by status
	 * 
	 * @param status
	 * @return
	 */
	public static String getStatusName(Integer status) {
		if (status == null)
			return "";
		switch (status) {
		case 0:
			return "Mới tới";
		case 1:
			return "Đang thực hiện";
		case 2:
			return "Từ chối";
		case 3:
			return "Hoàn thành (Chờ đánh giá)";
		case 4:
			return "Đóng";
		case 5:
			return "Bị thu hồi";
		default:
			return "";
		}
	}
	
	public static String getDomain() {
		HttpServletRequest request = getCurrentRequest();
		if (request == null)
			return "";
		return request.getScheme() + "://" + request.getServerName() + ":" + request.getServerPort();
	}

	public static String getToken() {
		HttpServletRequest request = getCurrentRequest();
		return request != null ? AuthenticationProvider.getJwtFromRequest(request) : "";
	}
	
	private static RestResponse convertJsonStringToObject(String json) {
		ObjectMapper mapper = new ObjectMapper();
		try {
			return mapper.readValue(json, RestResponse.class);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
		}
		return null;
	}

	/**
	 * Get message
	 * 
	 * @param e1
	 * @return
	 */
	public static String getMessage(HttpClientErrorException e1, String messageDefault) {
		if (e1 == null || org.apache.commons.lang.StringUtils.isEmpty(e1.getResponseBodyAsString())) {
			return messageDefault;
		}

		RestResponse rs = convertJsonStringToObject(e1.getResponseBodyAsString());
		if (rs != null) {
			return rs.getMessage();
		}

		return messageDefault;
	}

	public static <T> T getValueOrDefault(T value, T defaultValue) {
		return value != null ? value : defaultValue;
	}

}
