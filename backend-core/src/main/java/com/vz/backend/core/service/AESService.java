package com.vz.backend.core.service;

import java.io.UnsupportedEncodingException;
import java.nio.charset.StandardCharsets;
import java.security.InvalidKeyException;
import java.security.Key;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.SecretKeySpec;

import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class AESService {

	private static final String ALG = "AES";

	public static byte[] encrypt(byte[] input, String info) throws NoSuchAlgorithmException, NoSuchPaddingException,
	InvalidKeyException, IllegalBlockSizeException, BadPaddingException {
		Key aesKey = new SecretKeySpec(getKey(info), ALG);
		Cipher cipher = Cipher.getInstance(ALG);

		cipher.init(Cipher.ENCRYPT_MODE, aesKey);
		byte[] encrypted = cipher.doFinal(input);
		log.info("encrypted...", new String(encrypted, StandardCharsets.UTF_8));
		return encrypted;
	}

	public static byte[] decrypt(byte[] input, String info) throws NoSuchAlgorithmException, NoSuchPaddingException,
	InvalidKeyException, IllegalBlockSizeException, BadPaddingException {
		Key aesKey = new SecretKeySpec(getKey(info), ALG);
		Cipher cipher = Cipher.getInstance(ALG);

		cipher.init(Cipher.DECRYPT_MODE, aesKey);
		return cipher.doFinal(input);
	}

	// key.length = 16
	// AES only supports key sizes of 16, 24 or 32 bytes
	public static byte[] getKey(String info) {
		MessageDigest sha = null;
		byte[] key;
		try {
			key = info.getBytes("UTF-8");
			sha = MessageDigest.getInstance("SHA-1");
			key = sha.digest(key);
			return Arrays.copyOf(key, 16);
		} catch (NoSuchAlgorithmException | UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		throw new RestExceptionHandler(Message.ENCRYPTION_ERROR);
	}
}
