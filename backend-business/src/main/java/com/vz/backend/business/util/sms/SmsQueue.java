package com.vz.backend.business.util.sms;

import java.io.IOException;
import java.util.LinkedList;
import java.util.Queue;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class SmsQueue {

	private static Boolean isAvaiable = null;
	private static ISmsService smsService = null;

	private static LoadingCache<Long, String> phoneCache = null;

	private static Queue<UidContentDto> infoQueue = new LinkedList<>();

	private static Thread runningThread = null;

	private static void sendMessage(String phone, String message) {
		if (Boolean.FALSE.equals(isAvaiable)) {
			log.info("fake: <{}>: {}", phone, message);
			return;
		}
		GsmDevice gsm = GsmDevice.getInstance();

		// init_ed
		try {
			gsm.sendMessage(phone, message);
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private static void setupService(ISmsService smsService) {
		if (SmsQueue.smsService == null) {
			SmsQueue.smsService = smsService;
			GsmDevice gsm = GsmDevice.getInstance(smsService.getPort());
			if (isAvaiable == null) {
				try {
					gsm.init();
					gsm.connect();
					isAvaiable = true;
				} catch (GsmException e) {
					isAvaiable = false;
				}
			}
			phoneCache = CacheBuilder.newBuilder().maximumSize(1000).expireAfterWrite(10, TimeUnit.MINUTES)
					.build(new CacheLoader<Long, String>() {
						@Override
						public String load(Long userId) {
							String result = smsService.getPhone(userId);
							if (result == null) {
								return "";
							}
							return result;
						}
					});
		}
	}

	public static void addQueue(Long userId, String content, ISmsService smsService) {
		if (userId == null || content == null) {
			log.error("null");
			return;
		}
		setupService(smsService);
		infoQueue.add(new UidContentDto(userId, content));
		if (runningThread == null || !runningThread.isAlive()) {
			runningThread = new Thread(() -> {
				log.info("new Thread send sms");
				while (!infoQueue.isEmpty()) {
					UidContentDto u = infoQueue.poll();
					try {
						String phone = phoneCache.get(u.getUserId());
						sendMessage(phone, u.getContent());
					} catch (ExecutionException e) {
						e.printStackTrace();
					}
				}
				log.info("end send sms thread");
				runningThread = null;
			});
			runningThread.start();
		}
	}
}
