package com.vz.backend.business.controller;

import java.nio.file.Files;

import javax.websocket.server.PathParam;

import com.vz.backend.core.util.StreamUtils;
import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.google.common.net.HttpHeaders;
import com.vz.backend.business.domain.AttachmentComment;
import com.vz.backend.business.domain.DocumentComment;
import com.vz.backend.business.service.AttachmentCommentService;
import com.vz.backend.business.service.DocumentCommentService;
import com.vz.backend.business.service.DocumentInTrackingService;
import com.vz.backend.business.service.DocumentService;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.core.service.IService;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/attachment_comment")
public class AttachmentCommentController extends BaseController<AttachmentComment> {
	@Autowired
	AttachmentCommentService relateService;

	@Autowired
	FilesStorageService storageService;

	@Autowired
	DocumentService docService;

	@Autowired
	CategoryService categoryService;

	@Autowired
	DocumentCommentService cmtService;
	
	@Autowired
	DocumentInTrackingService trackingService;
	
	@Autowired
	EncryptionService encryptService;

	@Override
	public IService<AttachmentComment> getService() {
		return relateService;
	}

	@PostMapping(value = "/addAttachmentComment/{objId}")
	public ResponseEntity<?> addAttachment(@RequestParam MultipartFile[] files, @PathVariable Long objId) {

		// document is done still comment
		DocumentComment cmt = cmtService.findByIdCmt(objId);
		return new ResponseEntity<>(relateService.addListAttachmentComment(files, objId), HttpStatus.OK);
	}

	@PostMapping("/updateFile/{id}")
	public ResponseEntity<AttachmentComment> updateFile(@PathVariable Long id, @PathParam("file") MultipartFile file) {
		if (file == null) {
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		AttachmentComment attachmentComment = relateService.updateFile(id, file);
		return new ResponseEntity<>(attachmentComment, HttpStatus.OK);
	}

	@PostMapping(value = "/deleteById/{id}")
	public ResponseEntity<AttachmentComment> deleteAttachmentsById(@PathVariable Long id) {
		relateService.deleteById(id);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@GetMapping("/download/{name:.+}")
	public ResponseEntity<StreamingResponseBody> getFile(@PathVariable String name) {
		name = StringUtils.decodeFromUrl(name);
		AttachmentComment a = relateService.validDownloadFile(name);
		StreamingResponseBody stream;
		String outputName;
		if (Boolean.TRUE.equals(a.getEncrypt())) {
			stream = outputStream -> {
				try {
					encryptService.load(outputStream, a.getName());
				} finally {
					StreamUtils.closeOutputStream(outputStream);
				}
			};
			outputName = FilenameUtils.removeExtension(FilesStorageService.origin(name)) + ".zip";
		} else {
			Resource file = storageService.load(name);
			if (file == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}
			stream = outputStream -> {
				try {
					Files.copy(file.getFile().toPath(), outputStream);
				} finally {
					StreamUtils.closeOutputStream(outputStream);
				}
			};
			outputName = FilesStorageService.origin(name);
		}
		trackingService.trackingForDownloadFile(name, Constant.TYPE_ATT_CMT);
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + outputName + "\"").body(stream);
	}
}
