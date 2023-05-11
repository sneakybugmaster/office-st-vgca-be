package com.vz.backend.util;

public class Utils {

	private Utils() {

	}

	public static <T> T coalesce(T... list) {
		for (T i : list) {
			if (i != null) {
				return i;
			}
		}
		return null;
	}
}
