package com.vz.backend.core.service;

import java.util.List;

import javax.mail.MessagingException;
import javax.mail.internet.MimeMessage;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.ClassPathResource;
import org.springframework.mail.MailException;
import org.springframework.mail.SimpleMailMessage;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.mail.javamail.MimeMessageHelper;
import org.springframework.stereotype.Service;

import com.vz.backend.core.domain.Mail;

import lombok.extern.slf4j.Slf4j;


@Slf4j
@Service
public class MailService {
	private JavaMailSender javaMailSender;

	@Autowired
	public MailService(JavaMailSender javaMailSender) {
		this.javaMailSender = javaMailSender;
	}

	public void sendEmail(String emailTo, String subject, String message) throws MailException {
		SimpleMailMessage mail = new SimpleMailMessage();

		mail.setTo(emailTo);
		mail.setSubject(subject);
		mail.setText(message);

		this.javaMailSender.send(mail);
	}

	public void sendEmail(Mail email) throws MailException {
		SimpleMailMessage mail = new SimpleMailMessage();

		mail.setTo(toArray(email.getEmailTo()));
		mail.setCc(toArray(email.getEmailCc()));
		mail.setBcc(toArray(email.getEmailBcc()));
		mail.setSubject(email.getSubject());
		mail.setText(email.getMessage());

		this.javaMailSender.send(mail);
	}

	public void sendEmailWithAttachment(String emailTo, String subject, String message, String fileUrl)
			throws MailException, MessagingException {
		MimeMessage mimeMessage = javaMailSender.createMimeMessage();
		MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true);

		helper.setTo(emailTo);
		helper.setSubject(subject);
		helper.setText(message);

		this.javaMailSender.send(mimeMessage);
	}

	public void sendEmailWithAttachment(Mail email) throws MailException, MessagingException {
		MimeMessage mimeMessage = javaMailSender.createMimeMessage();
		MimeMessageHelper helper = new MimeMessageHelper(mimeMessage, true);

		helper.setTo(toArray(email.getEmailTo()));
		helper.setCc(toArray(email.getEmailCc()));
		helper.setBcc(toArray(email.getEmailBcc()));
		helper.setSubject(email.getSubject());
		helper.setText(email.getMessage());

		for (String file : email.getFiles()) {
			ClassPathResource classPathResource = new ClassPathResource(file);
			helper.addAttachment(classPathResource.getFilename(), classPathResource);
		}

		this.javaMailSender.send(mimeMessage);
	}

	private String[] toArray(List<String> list) {
		return list.toArray(new String[0]);
	}

	public void sendMailOffical(List<Mail> mails) {
		try {
			for (Mail i : mails) {
				sendEmailBasic(i);
				log.info("Sent info to : " + i.getMailTo());
			}
		} 
		catch (Exception e) {
			log.info("When login mail fail : Please turn on https://www.google.com/settings/security/lesssecureapps");
			e.printStackTrace();
//			throw new RestExceptionHandler(Message.ERROR_SYS);
		}
	}

	public void sendEmailBasic(Mail email) throws MailException, MessagingException {
		MimeMessage message = javaMailSender.createMimeMessage();
		MimeMessageHelper helper = new MimeMessageHelper(message, true);
		helper.setTo(email.getMailTo());
		helper.setSubject(email.getSubject());
		helper.setText(email.getContent(), true);
		this.javaMailSender.send(message);
	}
}
