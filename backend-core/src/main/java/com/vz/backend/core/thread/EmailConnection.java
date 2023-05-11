package com.vz.backend.core.thread;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.vz.backend.core.domain.Mail;
import com.vz.backend.core.service.MailService;

import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class EmailConnection implements Runnable {
	
	@Setter
	List<Mail> mails;
	
	@Autowired
	MailService mailService;
	
	@Override
	public void run() {
		try {
			log.info("Start send email ...");
			mailService.sendMailOffical(mails);
			log.info("End send email!");
		} catch (Exception e) {
			e.printStackTrace();
			log.info("Lỗi hệ thống trong quá trình gửi mail");
		}
	}
}
