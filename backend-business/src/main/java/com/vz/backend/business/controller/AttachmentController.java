package com.vz.backend.business.controller;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;

import javax.servlet.http.HttpServletResponse;
import javax.websocket.server.PathParam;

import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.MediaTypeFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.util.StreamUtils;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.google.common.net.HttpHeaders;
import com.vz.backend.business.domain.Attachment;
import com.vz.backend.business.service.AttachmentService;
import com.vz.backend.business.service.DocumentInTrackingService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.core.service.IService;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/attachment")
public class AttachmentController extends BaseController<Attachment> {
	@Autowired
	private AttachmentService attachmentService;

	@Autowired
	FilesStorageService storageService;

	@Autowired
	DocumentInTrackingService trackingService;
	
	@Autowired
	EncryptionService encryptService;

	@Override
	public IService<Attachment> getService() {
		return attachmentService;
	}

	@PostMapping(value = "/addAttachment/{objId}")
	public ResponseEntity<?> addAttachment(@RequestParam MultipartFile[] files, @PathVariable String objId) {
		Long objIdL = BussinessCommon.castId(objId);
		if (objIdL == null) {
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
		}
		return new ResponseEntity<>(attachmentService.addListAttachment(files, objIdL), HttpStatus.OK);
	}

	@PostMapping(value = "/addAttachment")
	public ResponseEntity<?> addAttachment2(@RequestParam MultipartFile file) {
		return new ResponseEntity<>(attachmentService.add(file), HttpStatus.OK);
	}

	@PostMapping(value = "/updateAttachment/{idAtt}/{idDoc}")
	public ResponseEntity<?> updateAttachment(@PathVariable Long idAtt, @PathVariable Long idDoc, @RequestParam MultipartFile file) {
		return new ResponseEntity<>(attachmentService.update(idAtt, idDoc, file), HttpStatus.OK);
	}

	@PostMapping(value = "/deleteByDoc/{documentId}")
	public ResponseEntity<?> deleteAttachmentsByDoc(@PathVariable Long documentId) {
		try {
			attachmentService.deleteAllByDocId(documentId);
			return ResponseEntity.status(HttpStatus.OK).build();
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@PostMapping("/updateFile/{id}")
	public ResponseEntity<Attachment> updateFile(@PathVariable Long id, @PathParam("file") MultipartFile file) {
		if (file == null) {
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		Attachment attachment = attachmentService.updateFile(id, file);
		return new ResponseEntity<>(attachment, HttpStatus.OK);
	}

	@PostMapping(value = "/deleteById/{id}")
	public ResponseEntity<Attachment> deleteAttachmentsById(@PathVariable Long id) {
		attachmentService.deleteById(id);
		return new ResponseEntity<>(HttpStatus.OK);
	}


	public ResponseEntity<StreamingResponseBody> getFileOld(@PathVariable String name) {
		name = StringUtils.decodeFromUrl(name);
		Attachment a = attachmentService.validDownloadFile(name);
		StreamingResponseBody stream;
		String outputName;
		if (Boolean.TRUE.equals(a.getEncrypt())) {
			stream = outputStream -> {
				try {
					encryptService.load(outputStream, a.getName());
				} finally {
					com.vz.backend.core.util.StreamUtils.closeOutputStream(outputStream);
				}
			};
			outputName =  FilenameUtils.removeExtension(FilesStorageService.origin(name)) + ".zip";
		} else {
			Resource file = storageService.load(name);
			if (file == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			stream = outputStream -> {
				try {
					Files.copy(file.getFile().toPath(), outputStream);
				} finally {
					com.vz.backend.core.util.StreamUtils.closeOutputStream(outputStream);
				}
			};
			outputName = FilesStorageService.origin(name);
		}
		trackingService.trackingForDownloadFile(name, Constant.TYPE_ATT);
		return ResponseEntity.ok().header(HttpHeaders.CONTENT_DISPOSITION,
				"attachment; filename=\"" + outputName + "\"").body(stream);
	}
	@GetMapping("/download/{name:.+}")
	public ResponseEntity<Resource> getFile(@PathVariable String name, HttpServletResponse response) throws IOException {
		name = StringUtils.decodeFromUrl(name);
		Attachment a = attachmentService.validDownloadFile(name);
		Resource file = null;
		String outputName;
		long lengthFile = 0;
		if (Boolean.TRUE.equals(a.getEncrypt())) {
			outputName = FilenameUtils.removeExtension(FilesStorageService.origin(name)) + ".zip";
		} else {
			file = storageService.load(name);
			if (file == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			outputName = FilesStorageService.origin(name);
			lengthFile = file.contentLength();
		}
		trackingService.trackingForDownloadFile(name, Constant.TYPE_ATT);

		response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + outputName + "\"");
		if (lengthFile > 0) {
			response.setContentLengthLong(lengthFile);
		}
		if (file != null) {
			MediaType mediaType = MediaTypeFactory.getMediaType(file).orElse(MediaType.APPLICATION_OCTET_STREAM);
			response.setContentType(mediaType.toString());
			try (InputStream is = file.getInputStream();
				 OutputStream os = response.getOutputStream()) {
				StreamUtils.copy(is, os);
				return ResponseEntity.ok().build();
			}
		} else {
			response.setContentType(MediaType.APPLICATION_OCTET_STREAM_VALUE);
			try (OutputStream outputStream = response.getOutputStream()) {
				encryptService.load(outputStream, a.getName());
				outputStream.flush();
				return ResponseEntity.ok().build();
			}
		}
	}

}
