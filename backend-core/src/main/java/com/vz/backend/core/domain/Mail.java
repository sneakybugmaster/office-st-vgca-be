package com.vz.backend.core.domain;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.vz.backend.core.common.BussinessCommon;

import lombok.Getter;
import lombok.Setter;
import lombok.extern.slf4j.Slf4j;

@Getter
@Setter
@Slf4j
public class Mail {
	private String subject;
	private String message;
	private List<String> emailTo;
	private List<String> emailCc;
	private List<String> emailBcc;
	private List<String> files;
	private List<String> contents;
	
	//for person
	private String mailTo;
	private String content;
	
	public Mail() {
		this.subject = null;
		this.message = null;
		this.emailTo = null;
		this.emailCc = null;
		this.emailBcc = null;
		this.files = null;
	}
	
	public static List<String> validMail(List<String> emailList) {
		if (BussinessCommon.isEmptyList(emailList)) return Collections.emptyList();
		List<String> rsList = new ArrayList<>();
		emailList.forEach(i -> {
			if (BussinessCommon.isEmail(i)) {
				rsList.add(i);
			} else {
				log.info("Email "+ i + " is invalid !");
			}
		});
		return rsList;
	}
	
	public Mail(String subject, List<String> emailTo, List<String> content) {
		super();
		this.subject = subject;
		this.emailTo = emailTo;
		this.contents = content;
	}

	public Mail(String subject, String mailTo, String content) {
		super();
		this.subject = subject;
		this.mailTo = mailTo;
		this.content = content;
	}
}
