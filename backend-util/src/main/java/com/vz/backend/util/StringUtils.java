package com.vz.backend.util;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import org.apache.commons.lang.StringEscapeUtils;

public class StringUtils {
	public interface ValidFunction<T> {
		T toObj(String s);

		default boolean valid(T arg1) {
			return true;
		}
	}


	private StringUtils() {
	}

	private static final Random RD = new Random();
	private static final String A2Z = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
	private static final String FONT_VN = "UTF-8";

	/** codau. */
	private static final char[] VIET_CHARS = { 'à', 'á', 'ả', 'ã', 'ạ', 'ă', 'ằ', 'ắ', 'ẳ', 'ẵ', 'ặ', 'â', 'ầ', 'ấ',
			'ẩ', 'ẫ', 'ậ', 'À', 'Á', 'Ả', 'Ã', 'Ạ', 'Ă', 'Ằ', 'Ắ', 'Ẳ', 'Ẵ', 'Ặ', 'Â', 'Ầ', 'Ấ', 'Ẩ', 'Ẫ', 'Ậ', 'è',
			'é', 'ẻ', 'ẽ', 'ẹ', 'ê', 'ề', 'ế', 'ể', 'ễ', 'ệ', 'È', 'É', 'Ẻ', 'Ẽ', 'Ẹ', 'Ê', 'Ề', 'Ế', 'Ể', 'Ễ', 'Ệ',
			'ì', 'í', 'ỉ', 'ĩ', 'ị', 'Ì', 'Í', 'Ỉ', 'Ĩ', 'Ị', 'ò', 'ó', 'ỏ', 'õ', 'ọ', 'ô', 'ồ', 'ố', 'ổ', 'ỗ', 'ộ',
			'ơ', 'ờ', 'ớ', 'ở', 'ỡ', 'ợ', 'Ò', 'Ó', 'Ỏ', 'Õ', 'Ọ', 'Ô', 'Ồ', 'Ố', 'Ổ', 'Ỗ', 'Ộ', 'Ơ', 'Ờ', 'Ớ', 'Ở',
			'Ỡ', 'Ợ', 'ù', 'ú', 'ủ', 'ũ', 'ụ', 'ư', 'ừ', 'ứ', 'ử', 'ữ', 'ự', 'Ù', 'Ú', 'Ủ', 'Ũ', 'Ụ', 'ỳ', 'ý', 'ỷ',
			'ỹ', 'ỵ', 'Ỳ', 'Ý', 'Ỷ', 'Ỹ', 'Ỵ', 'đ', 'Đ', 'Ư', 'Ừ', 'Ử', 'Ữ', 'Ứ', 'Ự' };

	/** khongdau. */
	private static final char[] NORMAL_CHARS = { 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a',
			'a', 'a', 'a', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'A', 'e',
			'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'e', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E', 'E',
			'i', 'i', 'i', 'i', 'i', 'I', 'I', 'I', 'I', 'I', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o', 'o',
			'o', 'o', 'o', 'o', 'o', 'o', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O', 'O',
			'O', 'O', 'u', 'u', 'u', 'u', 'u', 'u', 'u', 'u', 'u', 'u', 'u', 'U', 'U', 'U', 'U', 'U', 'y', 'y', 'y',
			'y', 'y', 'Y', 'Y', 'Y', 'Y', 'Y', 'd', 'D', 'U', 'U', 'U', 'U', 'U', 'U' };

	private static final String DATE_FORMAT = "yyyy-MM-dd";

	public static String toOracleSearchLikeSuffix(String searchText) {
		return toOracleSearchLike(searchText);
	}

	public static String toOracleSearchLike(String searchText) {
		String escapeChar = "/";
		String[] arrSpPat = { "/", "%", "_" };

		for (String str : arrSpPat) {
			if (!StringUtils.isNullOrEmpty(searchText)) {
				searchText = searchText.replaceAll(str, escapeChar + str);
			}
		}
		searchText = "%" + searchText + "%";
		return searchText;
	}

	/**
	 * Method to escape sql injection %, _, '
	 *
	 * @param searchText
	 * @return
	 */
	public static String toEscapeSqlSearchLike(String searchText) {
		searchText = StringEscapeUtils.escapeSql(searchText);
		searchText = searchText.replace("%", "\\%").replace("_", "\\_");
		searchText = "%" + searchText + "%";
		return searchText;
	}

	public static boolean isNullOrEmpty(final String s) {
		return isNullOrEmpty(s, true);
	}

	public static boolean isNullOrEmpty(final String s, boolean trim) {
		if (s == null || s.isEmpty()) {
			return true;
		}

		return trim && s.trim().isEmpty();
	}

	public static String arrayToDelimitedString(String[] arr, String delim) {
		if (arr == null || arr.length == 0) {
			return "";
		}

		if (arr.length == 1) {
			return arr[0] == null ? "" : arr[0];
		}

		StringBuilder sb = new StringBuilder();
		for (int i = 0; i < arr.length; i++) {
			if (i > 0) {
				sb.append(delim);
			}
			if (arr[i] != null) {
				sb.append(arr[i]);
			}
		}

		return sb.toString();
	}

	public static String getSearchableString(String input) {
		if (isNullOrEmpty(input)) {
			return "";
		}

		input = input.trim();
		for (int i = 0; i < VIET_CHARS.length; i++) {
			input = input.replace(VIET_CHARS[i], NORMAL_CHARS[i]);
		}

		return input;
	}

	/**
	 * Đây là hàm kiểm tra xem chuỗi truyền vào là những ký số không.
	 *
	 * @param String str
	 * @return boolean
	 */
	public static boolean isDigitString(String str) {
		if (isNullOrEmpty(str)) {
			return false;
		}
		char[] arrChar = str.toCharArray();
		for (char element : arrChar) {
			if (!Character.isDigit(element)) {
				return false;
			}
		}
		return true;
	}

	/**
	 * @Description: Kiểm tra có phải là số Integer không
	 * @Author: pmdn_ducln1
	 * @Time: Jan 4, 2013
	 * @param str
	 * @return
	 */
	public static boolean isInteger(String str) {
		if (isNullOrEmpty(str)) {
			return false;
		}
		try {
			int value = Integer.parseInt(str);
			return true;
		} catch (Exception e) {
			return false;
		}

	}

	/**
	 * Kiem tra 1 chuoi co phai la so nguyen duong hay khong?
	 *
	 * @author OanhLT1
	 * @param str
	 * @return
	 */
	public static boolean isPositiveLong(String str) {
		if (isNullOrEmpty(str)) {
			return false;
		}

		try {
			long value = Long.parseLong(str);
			return value >= 0;
		} catch (NumberFormatException e) {
			return false;
		}

	}

	public static boolean isValidDate(String date) {
		SimpleDateFormat dateFormat = new SimpleDateFormat(DATE_FORMAT);
		dateFormat.setLenient(false);
		try {
			dateFormat.parse(date);
			return true;
		} catch (ParseException e) {
			return false;
		}
	}

	public static String cutAllSpace(String st) {
		if (!isNullOrEmpty(st)) {
			return st.replaceAll("\\s", "");
		}
		return null;
	}

	public static String handleSubmit(String s) {
		if (s == null) {
			return null;
		}
		String trim = s.trim();
		if (trim.length() == 0) {
			return null;
		}
		return trim.toLowerCase();
	}

	public static String randomPassword(int length) {
		StringBuilder sb = new StringBuilder(length);
		for (int i = 0; i < length; i++) {
			sb.append(A2Z.charAt(RD.nextInt(A2Z.length())));
		}
		return sb.toString();
	}

	public static String decodeFromUrl(String str) {
		if (isNullOrEmpty(str)) {
			return null;
		}

		try {
			return URLDecoder.decode(str, FONT_VN);
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}

		return null;
	}

	public static StringBuilder padLeft(StringBuilder inputString, int length, char c) {
		if (inputString.length() >= length) {
			return inputString;
		}
		for (int i = length - inputString.length(); i > 0; --i) {
			inputString.append(c);
		}

		return inputString;
	}

	public static <T> List<T> parse(String input, ValidFunction<T> validator) {
		if (input == null) {
			new ArrayList<>();
		}

		List<T> results = new ArrayList<>();
		for (String s : input.split(",")) {
			if (!"".equals(s)) {
				T tmp;
				try {
					tmp = validator.toObj(s);
				} catch (Exception e) {
					throw new ParseListException("Không thể chuyển giá trị: `" + s + "`");
				}
				if (!validator.valid(tmp)) {
					throw new ParseListException("Giá trị không hợp lệ: `" + s + "`");
				}
				results.add(tmp);
			}
		}
		return results;
	}
	
	public static String castToAcronym(String str) {
		return str.replaceAll("\\B.\\s?", "").toUpperCase();
	}

	public static String listStringToDelimitedString(List<String> stringList, String delimiter) {
		if (stringList == null) {
			return null;
		}
		StringBuilder result = new StringBuilder();
		for (int i = 0; i < stringList.size(); i++) {
			if (i < stringList.size() - 1) {
				result.append(stringList.get(i)).append(delimiter);
			} else {
				result.append(stringList.get(i));
			}
		}
		return result.toString();
	}

	public static String trimRepeatingSpaces(String input) {
		if (input != null && !input.isEmpty()) {
			input = input.replaceAll("\\s+", " ");
		}
		return input;
	}
}
