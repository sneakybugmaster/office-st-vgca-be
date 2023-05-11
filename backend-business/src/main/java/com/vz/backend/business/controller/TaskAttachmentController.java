package com.vz.backend.business.controller;

import java.nio.file.Files;

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
import com.vz.backend.business.domain.Task;
import com.vz.backend.business.domain.TaskAttachment;
import com.vz.backend.business.domain.TaskComment;
import com.vz.backend.business.domain.WordEditorProcess;
import com.vz.backend.business.service.TaskAttachmentService;
import com.vz.backend.business.service.TaskCommentService;
import com.vz.backend.business.service.TaskService;
import com.vz.backend.business.service.WordEditorService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/taskAtt")
public class TaskAttachmentController {
	@Autowired
	TaskAttachmentService service;

	@Autowired
	FilesStorageService storageService;

	@Autowired
	TaskService taskService;

	@Autowired
	TaskCommentService taskCmtService;
	
	@Autowired
	WordEditorService weService;
	@Autowired
	EncryptionService encryptService;
	
	private WordEditorProcess process = null;

	@PostMapping(value = "/add/{objId}")
	public ResponseEntity<?> addAttachment(@RequestParam MultipartFile[] files, @PathVariable Long objId,
			@RequestParam Long type) {
//		validObjIdByType(type, objId);
		if (Constant.TYPE_WORD_EDITOR.equals(type) && this.process != null) {
			return new ResponseEntity<>(service.addListAttachment(files, this.process.getId(), type), HttpStatus.OK);
		}
		
		return new ResponseEntity<>(service.addListAttachment(files, objId, type), HttpStatus.OK);
	}

	/**
	 * valid type of object id type = 1 : Task / type = 2: Task comment
	 *
	 * @param type
	 * @param objId
	 */
	private void validObjIdByType(Long type, Long objId) {
		Long clientId = BussinessCommon.getClientId();
		if (Constant.TYPE_TASK.equals(type)) { // task
			Task task = taskService.findByClientIdAndId(clientId, objId);
			if (task == null || !task.getActive()) {
				throw new RestExceptionHandler(Message.TASK_NOT_FOUND);
			}
		} else if (Constant.TYPE_TASK_CMT.equals(type)) { // task comment
			TaskComment taskComment = taskCmtService.findByClientIdAndId(clientId, objId);
			if (taskComment == null || Boolean.FALSE.equals(taskComment.getActive())) {
				throw new RestExceptionHandler(Message.TASK_CMT_NOT_FOUND);
			}
		} else if (Constant.TYPE_WORD_EDITOR.equals(type)) { // word editor
			this.process = weService.getProcess(BussinessCommon.getUserId(), objId);
			if (this.process == null)
				throw new RestExceptionHandler(Message.DOCUMENT_NOT_FOUND);
		} else {
			throw new RestExceptionHandler(Message.NOT_FOUND_OBJECT_DATA);
		}
	}

	@GetMapping("/download/{name:.+}")
	public ResponseEntity<StreamingResponseBody> getFile(@PathVariable String name) {
		name = StringUtils.decodeFromUrl(name);
		TaskAttachment a = service.validDownloadFile(name);
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
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + outputName + "\"").body(stream);
	}

	@PostMapping(value = "/deleteById/{id}")
	public ResponseEntity<TaskAttachment> deleteAttachmentsById(@PathVariable Long id) {
		TaskAttachment att = service.findByClientIdAndId(BussinessCommon.getClientId(), id);
		if (att != null) {
			storageService.deleteFile(att.getName());
			service.deleteById(id);
		}
		return new ResponseEntity<>(HttpStatus.OK);
	}
}
