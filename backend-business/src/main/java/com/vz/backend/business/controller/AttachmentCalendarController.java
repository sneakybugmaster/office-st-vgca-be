package com.vz.backend.business.controller;

import java.nio.file.Files;
import java.util.List;

import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.exception.RestForbidden;
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
import com.vz.backend.business.domain.AttachmentCalendar;
import com.vz.backend.business.domain.AttachmentComment;
import com.vz.backend.business.domain.Calendar2;
import com.vz.backend.business.domain.CalendarComment;
import com.vz.backend.business.service.AttachmentCalendarService;
import com.vz.backend.business.service.Calendar2Service;
import com.vz.backend.business.service.CalendarCommentService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.AuthorityEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ObjTypeEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.AuthorityUserService;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.core.service.RoleService;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/attachment_calendar")
public class AttachmentCalendarController {
	
	@Autowired
	Calendar2Service calendarService;
	
	@Autowired
	AttachmentCalendarService attService;
	
	@Autowired
	CalendarCommentService cmtService;
	
	@Autowired
	FilesStorageService storageService;
	
	@Autowired
	AuthorityUserService authoService;
	
	@Autowired
	RoleService rService;
	
	@Autowired
	EncryptionService encryptService;

	@Autowired
	private RoleService roleService;

	private void checkPermission() {
		if (roleService.existUserInModule(ModuleCodeEnum.USER_MANUAL_CONFIG.getName())) {
			return;
		}
		throw new RestForbidden(Message.NO_ACTION_PERMISSION);
	}

	@PostMapping(value = "/addAttachment/{objId}/{objType}")
	public ResponseEntity<?> addAttachment(@RequestParam MultipartFile[] files, @PathVariable Long objId,
			@PathVariable int objType, @RequestParam(required = false, defaultValue = "0") int week,
			@RequestParam(required = false, defaultValue = "0") int year) {
		User user = BussinessCommon.getUser();
		Long userId = user.getId();
		ObjTypeEnum type = null; 
		if(objType == 1) {
			type = ObjTypeEnum.CALENDAR;
			Calendar2 c = calendarService.findByIdAndClientIdAndActive(objId, true);
			if(c == null) throw new RestExceptionHandler(Message.CALENDAR_INVALD);
		} else if(objType == 2) {
			type = ObjTypeEnum.CALENDAR_CMT;
			CalendarComment c = cmtService.getById(objId);
			if(c == null) throw new RestExceptionHandler(Message.TASK_CMT_NOT_FOUND);
		} else if (objType == 3) {
			if (!authoService.isUserHasAuthority(userId, null,
					AuthorityEnum.APPROVE_TOP_LEVEL_CALENDAR)
					&& !authoService.isUserHasAuthority(userId, null,
							AuthorityEnum.APPROVE_UNIT_LEVEL_CALENDAR))
				throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);
			objId = 0L;
			type = ObjTypeEnum.CALENDAR_WEEK;
		} else if(objType == 4 ) {
			if(!rService.isSupervisor(user)) {
				throw new RestExceptionHandler(Message.NO_ACTION_PERMISSION);
			}
			type = ObjTypeEnum.MODULE;
		} else {
			checkPermission();
			type = ObjTypeEnum.MODULE;
		}
		
		return new ResponseEntity<>(attService.addListAttachment(files, objId, type, week, year), HttpStatus.OK);
	}

	@PostMapping(value = "/deleteById/{id}")
	public ResponseEntity<AttachmentComment> deleteAttachmentsById(@PathVariable Long id) {
		attService.deleteById(id);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@GetMapping("/download/{name:.+}")
	public ResponseEntity<StreamingResponseBody> getFile(@PathVariable String name) {
		name = StringUtils.decodeFromUrl(name);
		AttachmentCalendar a = attService.validDownloadFile(name);
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

	/**
	 * get file user manual
	 * @param objId
	 * @return
	 */
	@GetMapping(value = "/download/{objId}/cat")
	public ResponseEntity<Resource> getFile(@PathVariable Long objId) {
		Resource file = attService.downloadByIdCat(objId);
		if (file == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}

		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + FilesStorageService.origin(file.getFilename()) + "\"")
				.body(file);
	}

	@GetMapping(value = "/getExtensionLoadByIdCat/{objId}/cat")
	public ResponseEntity<?> getExtensionLoadByIdCat(@PathVariable Long objId) {
		List<String> name = attService.getExtensionLoadByIdCat(objId);
		if (name == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		return new ResponseEntity<>(name, HttpStatus.OK);
	}
}
