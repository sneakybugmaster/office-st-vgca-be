package com.vz.backend.core.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.FileService;

import lombok.extern.slf4j.Slf4j;

@RestController
@RequestMapping("/file")
@Slf4j
public class ApiFileController {
	
	@Value("${ocr.domain}")
	private String domain;
	
	@Autowired
	private FileService fileService;

	@PostMapping("/ocr")
	public ResponseEntity<Object> doOcr(@RequestParam(required = true) MultipartFile file) {
		log.info("Domain OCR info : " + domain);
		try {
			Resource invoicesResource = fileService.getResource(file);

			LinkedMultiValueMap<String, Object> parts = new LinkedMultiValueMap<>();
			parts.add("file", invoicesResource);

			HttpHeaders httpHeaders = new HttpHeaders();
			httpHeaders.setContentType(MediaType.MULTIPART_FORM_DATA); 

			HttpEntity<LinkedMultiValueMap<String, Object>> requestEntity = new HttpEntity<>(parts, httpHeaders);

			RestTemplate restTemplate = new RestTemplate();
			Object object = restTemplate.postForObject(domain, requestEntity, Object.class);
			log.info("File OCR info : " + file.getResource().getFilename());

			return new ResponseEntity<>(object, HttpStatus.OK);
		} catch (Exception e) {
			e.printStackTrace();
			throw new RestExceptionHandler(Message.ERROR_SYS);
		}
	}
}
