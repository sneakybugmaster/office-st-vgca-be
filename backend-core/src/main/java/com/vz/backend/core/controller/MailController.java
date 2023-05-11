package com.vz.backend.core.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.mail.MailException;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.vz.backend.core.service.MailService;

@RestController
@RequestMapping("/mail")
public class MailController {
	@Autowired
	private MailService mailService;

	@PostMapping("/test")
	public ResponseEntity<String> testMail() {
		try {
			mailService.sendEmailWithAttachment("hoangtamqn@gmail.com", "Eco Office - Test mail",
					"Hello Tam, we are test mail function. Pls don't reply it. Thanks!", "");
			return new ResponseEntity<>("Email was send!", HttpStatus.OK);
		} catch (MailException e) {
			return new ResponseEntity<>("Error while send this email", HttpStatus.EXPECTATION_FAILED);
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}
}
