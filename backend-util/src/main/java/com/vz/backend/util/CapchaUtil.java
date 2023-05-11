package com.vz.backend.util;

import java.util.Random;

public class CapchaUtil {

	private static final Random RD = new Random();

	/**
	 * Generate a CAPTCHA String consisting of random lowercase & uppercase letters,
	 * and numbers.
	 */
	public static String generateCaptchaString() {
		int length = 7;// + (Math.abs(Math.random().nextInt()) % 3);
		StringBuilder captchaStringBuffer = new StringBuilder();
		for (int i = 0; i < length; i++) {
			int baseCharNumber = RD.nextInt() % 62;
			int charNumber;
			if (baseCharNumber < 26) {
				charNumber = 65 + baseCharNumber;
			} else if (baseCharNumber < 52) {
				charNumber = 97 + baseCharNumber - 26;
			} else {
				charNumber = 48 + baseCharNumber - 52;
			}
			captchaStringBuffer.append((char) charNumber);
		}
		return captchaStringBuffer.toString();
	}
}
