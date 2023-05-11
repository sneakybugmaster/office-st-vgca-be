package com.vz.backend.util;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

import org.springframework.lang.Nullable;
import org.springframework.security.crypto.password.PasswordEncoder;

public class PasswordUtils {
	private static final String DEFAULT_PW = "123456a@";
	private static final String CHAR_LOWERCASE = "abcdefghijklmnopqrstuvwxyz";
	private static final String CHAR_UPPERCASE = CHAR_LOWERCASE.toUpperCase();
	private static final String DIGIT = "0123456789";
//	private static final String OTHER_PUNCTUATION = "!@#&()–[{}]:;',?/*";
//	private static final String OTHER_SYMBOL = "~$^+=<>";
//	private static final String OTHER_SPECIAL = OTHER_PUNCTUATION + OTHER_SYMBOL;
	private static final int PASSWORD_LENGTH = 6;
	private static SecureRandom random = new SecureRandom();

	public static String encode(PasswordEncoder encoder, String key) {
		String value = encoder.encode(key);
		return value;
	}

	public static String getDefaultPassword(PasswordEncoder encoder) {
		return encode(encoder, DEFAULT_PW);
	}

	public static String getPasswordEncode(PasswordEncoder encoder, String password) {
		return encode(encoder, password);
	}

	public static boolean matches(PasswordEncoder encoder, CharSequence rawPassword, String encodedPassword) {
		boolean isMactch = encoder.matches(rawPassword, encodedPassword);
		return isMactch;
	}

	public static String signName(@Nullable String name) {
		if (name == null) {
			return "";
		}
		Long exp = Calendar.getInstance().getTime().getTime() + MAX_EXP_FILE;

		return name + "?exp=" + exp + "&sign=" + md5(groupName(exp, name));
	}

	private static final long MAX_EXP_FILE = 8 * 60 * 60 * 1000L;

	public static String validSign(String name, Long exp, String sign) {
		long now = Calendar.getInstance().getTime().getTime();
		if (exp < now) {
			return "Link đã quá hạn";
		}

		if (md5(groupName(exp, name)).equals(sign)) {
			return null;
		}
		return "Sign không hợp lệ";
	}

	private static String groupName(Long date, String name) {
		return date + "-random-" + name;
	}

	private static final char[] HEX_ARRAY = "0123456789ABCDEF".toCharArray();

	public static String bytesToHex(byte[] bytes) {
		char[] hexChars = new char[bytes.length * 2];
		for (int j = 0; j < bytes.length; j++) {
			int v = bytes[j] & 0xFF;
			hexChars[j * 2] = HEX_ARRAY[v >>> 4];
			hexChars[j * 2 + 1] = HEX_ARRAY[v & 0x0F];
		}
		return new String(hexChars);
	}

	private static String md5(String code) {
		if (code == null) {
			return "error: null";
		}
		try {
			MessageDigest md = MessageDigest.getInstance("MD5");
			md.update(code.getBytes());

			return bytesToHex(md.digest());
		} catch (NoSuchAlgorithmException e) {
			return "error: NoSuchAlgorithmException";
		}
	}
	
	/**
	 * Gen random string by condition
	 * Ex: at least 2 special characters : genRandomStr(OTHER_SPECIAL, 2)
	 * @param random
	 * @param input
	 * @param size
	 * @return
	 */
	private static String genRandomStr(String input, int size) {
		if (input == null || input.length() <= 0)
			throw new IllegalArgumentException("Invalid input.");
		if (size < 1)
			throw new IllegalArgumentException("Invalid size.");

		StringBuilder result = new StringBuilder(size);
		for (int i = 0; i < size; i++) {
			// produce a random order
			int index = random.nextInt(input.length());
			result.append(input.charAt(index));
		}
		return result.toString();
	}

	/**
	 * Make it more random
	 * @param input
	 * @return
	 */
	private static String shuffle(String input) {
		List<String> result = Arrays.asList(input.split(""));
		Collections.shuffle(result);
		return result.stream().collect(Collectors.joining());
	}

	/**
	 * Auto gen password
	 * Rule : 
	 * 1. least 1 chars (lowercase) 
	 * 2. least 1 chars (uppercase) 
	 * 3. least 2 digits
	 * @return
	 */
	public static String autoGenPw() {
		StringBuilder result = new StringBuilder(PASSWORD_LENGTH);
		
		// at least 1 chars (lowercase)
		String strLowerCase = genRandomStr(CHAR_LOWERCASE, 1);
		result.append(strLowerCase);

		// at least 1 chars (uppercase)
		String strUppercaseCase = genRandomStr(CHAR_UPPERCASE, 1);
		result.append(strUppercaseCase);

		// at least 2 digits
		String strDigit = genRandomStr(DIGIT, 4);
		result.append(strDigit);

		// shuffle again
		return shuffle(result.toString());
	}
}
