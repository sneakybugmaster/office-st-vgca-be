package com.vz.backend.core.service;

import java.util.regex.Pattern;

import javax.mail.internet.MimeMessage;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.mail.javamail.MimeMessagePreparator;
import org.springframework.stereotype.Service;
import org.thymeleaf.context.Context;
import org.thymeleaf.spring5.SpringTemplateEngine;

import com.vz.backend.core.dto.MailNotiDto;


@Service
public class MailNotiService {
	
	private static Pattern PATTERN = Pattern.compile("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,4}");

	@Autowired
	private JavaMailSender javaMailSender;
	@Autowired
	private SpringTemplateEngine templateEngine;
	
	@Value("${configs.mail: ''}")
	private String mail;

	public void sendNoti(MailNotiDto dto) throws Exception {
		if (dto.getEmail() == null || !PATTERN.matcher(dto.getEmail()).matches()) {
			return;
		}
		MimeMessagePreparator mmp = new MimeMessagePreparator() {
			@Override
			public void prepare(MimeMessage mimeMessage) throws Exception {
				Context context = new Context();
				context.setVariable("preview", dto.getPreview());
				context.setVariable("position", dto.getPosition());
				context.setVariable("fullName", dto.getFullName());
				context.setVariable("docType", dto.getDocType());
				String html = templateEngine.process("mail-noti", context);

				MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true, "UTF-8");
				helper.setFrom(mail);
				helper.setTo(dto.getEmail());
				helper.setSubject("[QLVB] Thông báo " + dto.getDocType());
				helper.setText(html, true);

			}
		};
		
		javaMailSender.send(mmp);
	}
	
	public void sendNotiHasException(MailNotiDto dto) {
		try {
			sendNoti(dto);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
