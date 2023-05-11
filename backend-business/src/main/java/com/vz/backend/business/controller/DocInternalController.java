package com.vz.backend.business.controller;

import java.nio.file.Files;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import com.vz.backend.core.util.StreamUtils;
import org.apache.commons.io.FilenameUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.DateTimeFormat.ISO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.google.common.net.HttpHeaders;
import com.vz.backend.business.domain.documentInternal.DocInternalAttach;
import com.vz.backend.business.domain.documentInternal.DocInternalComment;
import com.vz.backend.business.dto.document.DocInCommentDto;
import com.vz.backend.business.dto.document.DocInternalCreateDto;
import com.vz.backend.business.service.docInternal.DocInternalAttachService;
import com.vz.backend.business.service.docInternal.DocInternalCommentService;
import com.vz.backend.business.service.docInternal.DocInternalService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.dto.ResponseUploadSignFile;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.DateTimeUtils;

@RestController
@RequestMapping("/doc_internal")
public class DocInternalController {
	enum SortBy {
		ID("id"), CREATEDATE("createDate"), // Ngày tạo
		UPDATEDATE("updateDate"), // Ngày cập nhật
		DATE_APPROVE("approveDate"), // Ngày duyệt
		NUMBERSIGN("numberOrSign"), // Số/Ký hiệu
		PREVIEW("preview"), // Trích yếu
		USER_ENTER("createUser.fullName"); // Người tạo

		private String field;

		private SortBy(String field) {
			this.field = field;
		}

		public static String getEnum(String name) {
			for (SortBy v : values()) {
				if (v.name().equals(name)) {
					return v.field;
				}
			}
			return SortBy.ID.field;
		}
	}
	@Autowired
	private DocInternalService docInternalService;
	
	@Autowired
	private DocInternalAttachService docInternalAttachService;
	
	@Autowired
	private DocInternalCommentService docInternalCommentService;

	@Autowired
	private FilesStorageService storageService;
	
	@Autowired
	private EncryptionService encryptService;
	
	@PostMapping("/add")
	public ResponseEntity<?> create(@RequestBody DocInternalCreateDto input) {
		return new ResponseEntity<>(docInternalService.add(input), HttpStatus.OK);
	}
	
	@PostMapping("/addFile/{docId}")
	public ResponseEntity<?> addFile(@PathVariable Long docId, @RequestParam MultipartFile[] fileND, @RequestParam(required = false) MultipartFile[] filePL) {
		return new ResponseEntity<>(docInternalService.addFile(docId, fileND, filePL), HttpStatus.OK);
	}

	@PostMapping("/update/{id}")
	public ResponseEntity<?> update(@PathVariable Long id, @RequestBody DocInternalCreateDto input) {
		return new ResponseEntity<>(docInternalService.update(id, input), HttpStatus.OK);
	}
	
	@PostMapping("/updateFile/{docId}")
	public ResponseEntity<?> updateFile(@PathVariable Long docId, @RequestParam(required = false) List<Long> deleteIds, @RequestParam(required = false) MultipartFile[] fileND, @RequestParam(required = false) MultipartFile[] filePL) {
		return new ResponseEntity<>(docInternalService.updateFile(docId, deleteIds, fileND, filePL), HttpStatus.OK);
	}

	@PostMapping("/delete/{id}")
	public ResponseEntity<?> delete(@PathVariable Long id) {
		return new ResponseEntity<>(docInternalService.delete(id), HttpStatus.OK);
	}

	@GetMapping("/getNumberOrSign")
	public ResponseEntity<?> getNumberOrSign() {
		return new ResponseEntity<>(docInternalService.getNumberOrSign(), HttpStatus.OK);
	}
	
	@GetMapping("/getDetailById/{id}")
	public ResponseEntity<?> getDetailById(@PathVariable Long id) {
		return new ResponseEntity<>(docInternalService.getDetailById(id), HttpStatus.OK);
	}
	
	@PostMapping("/approve/{id}")
	public ResponseEntity<?> approve(@PathVariable Long id, @RequestParam boolean accept, @RequestParam(defaultValue = "") String comment, 
			@RequestParam(required = false) MultipartFile[] files) {
		return new ResponseEntity<>(docInternalService.approve(id, accept, comment, files), HttpStatus.OK);
	}
	
	@GetMapping("/getListDocInternal")
	public ResponseEntity<?> getListDocInternal(@RequestParam int tab, @RequestParam(required = false) String numberOrSign, 
			@RequestParam(required = false) String preview, @RequestParam(required = false) String personEnter, 
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date createDate, @RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date approveDate,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date createFrom, @RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date createTo,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date approveFrom, @RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date approveTo,
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy, @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size, @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(docInternalService.getListDocInternal(tab, numberOrSign, preview, personEnter, createDate, createFrom, createTo, approveDate, approveFrom, approveTo, pageable), HttpStatus.OK);
	}

	@GetMapping("/getAllDocComplete")
	public ResponseEntity<?> getAllDocComplete(
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy, @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size, @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(docInternalService.getAllDocComplete(pageable), HttpStatus.OK);
	}
	
	@GetMapping("/getAllDocInternal")
	public ResponseEntity<?> getAllDocInternal(@RequestParam(required = false) Long tab,@RequestParam(required = false) String numberOrSign, @RequestParam(required = false) DocumentStatusEnum status,
			@RequestParam(required = false) String preview, @RequestParam(required = false) String personEnter, 
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date createDate, @RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date approveDate,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date createFrom, @RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date createTo,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date approveFrom, @RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date approveTo,
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy, @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size, @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		createFrom = DateTimeUtils.handleSubmit(createFrom, Calendar.MILLISECOND, -1);
		createTo = DateTimeUtils.handleSubmit(createTo, Calendar.DAY_OF_MONTH, 1);
		approveFrom = DateTimeUtils.handleSubmit(approveFrom, Calendar.MILLISECOND, -1);
		approveTo = DateTimeUtils.handleSubmit(approveTo, Calendar.DAY_OF_MONTH, 1);
		numberOrSign = BussinessCommon.convert(numberOrSign);
		preview = BussinessCommon.convert(preview);
		return new ResponseEntity<>(docInternalService.getAllDocInternal(tab, numberOrSign, status, preview, personEnter, createDate, createFrom, createTo, approveDate, approveFrom, approveTo, pageable), HttpStatus.OK);
	}
	
	@GetMapping("/getAttachs/{docId}")
	public ResponseEntity<?> getAttach(@PathVariable Long docId) {
		docInternalService.validatePermission(docId);
		return new ResponseEntity<>(docInternalAttachService.getListAttachByDocId(docId), HttpStatus.OK);
	}
	
	@GetMapping("report")
	public ResponseEntity<?> report() {
		return new ResponseEntity<>(docInternalService.report(), HttpStatus.OK);
	}

	@GetMapping("/download/{attachId}")
//	@ResponseBody
	public ResponseEntity<StreamingResponseBody> getFile(@PathVariable Long attachId) {
		DocInternalAttach attach = docInternalAttachService.getFileByIdOrFileName(attachId, null);
		String name = attach.getName();
		StreamingResponseBody stream;
		String outputName;
		if (Boolean.TRUE.equals(attach.getEncrypt())) {
			stream = outputStream -> {
				try {
					encryptService.load(outputStream, name);
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
	
	@GetMapping("/download/{attachId}/{fileName}")
//	@ResponseBody
	public ResponseEntity<StreamingResponseBody> getFile(@PathVariable Long attachId, @PathVariable String fileName) {
		return getFile(attachId);
	}
	
	@PostMapping("/update-sign-file/{attachId}")
	public ResponseEntity<ResponseUploadSignFile> updateSignWordFile(@RequestParam("uploadfile") MultipartFile file,
			@PathVariable long attachId) {
		ResponseUploadSignFile response = updateSign(file, attachId);
		
		return new ResponseEntity<>(response, HttpStatus.OK);
	}
	
	@PostMapping("/update-sign-file/{attachId}/hsm")
	public ResponseEntity<ResponseUploadSignFile> updateSignWordFileHSM(@RequestParam("file") MultipartFile file,
			@PathVariable long attachId) {
		ResponseUploadSignFile response = updateSign(file, attachId);
		
		return new ResponseEntity<>(response, HttpStatus.OK);
	}

	public ResponseUploadSignFile updateSign(MultipartFile file, long attachId) {
		ResponseUploadSignFile response = new ResponseUploadSignFile(true, "Successed", "", "");
		
		if (file == null) {
			response.setStatus(false);
			response.setMessage("Failed");
			return response;
		} else {
			response.setFileName(file.getOriginalFilename());
		}
		DocInternalAttach oDocAttach = docInternalAttachService.getAttachById(attachId);
		if (oDocAttach == null) {
			response.setStatus(false);
			response.setMessage("Failed");
			return response;
		} else {
			response.setFileServer(oDocAttach.getName());
		}
		MultipartFile[] files = {file};
		try {
			docInternalService.addSignFile(oDocAttach.getDocId(), files);
		} catch (Exception e) {
			response.setStatus(false);
			response.setMessage("Failed");
		}
		return response;
	}
	
	@GetMapping("/retake/{id}")
	public ResponseEntity<?> retake(@PathVariable Long id) {
		return new ResponseEntity<>(docInternalService.retake(id), HttpStatus.OK);
	}
	
	@PostMapping("/attachment/delete/{id}")
	public ResponseEntity<?> deleteAttachment(@PathVariable Long id) {
		docInternalAttachService.deleteById(id);
		return new ResponseEntity<>(HttpStatus.OK);
	}
	
	@PostMapping("/comment/{docId}")
	public ResponseEntity<DocInternalComment> addComment(@PathVariable Long docId, @RequestParam String comment) {
		return new ResponseEntity<>(docInternalCommentService.addComment(docId, comment), HttpStatus.OK);
	}
	
	@PostMapping("/comment/attachment")
	public ResponseEntity<List<DocInternalAttach>> addAttachmentComment(@RequestParam MultipartFile[] files, @RequestParam  Long commentId) {
		return new ResponseEntity<>(docInternalAttachService.addListAttachmentComment(files, commentId, false), HttpStatus.OK);
	}
	
	@GetMapping("/comment/load/{docId}")
	public ResponseEntity<List<DocInCommentDto>> getAllComments(@PathVariable Long docId) {
		return new ResponseEntity<>(docInternalCommentService.findDocInternalComments(docId), HttpStatus.OK);
	}

	@PostMapping("/completeNotApprove/{docId}")
	public ResponseEntity<?> completeNotApprove(@PathVariable Long docId, @RequestParam String comment, @RequestParam MultipartFile[] file) {
		return new ResponseEntity<>(docInternalService.completeNotApprove(docId, file, comment), HttpStatus.OK);
	}


	@GetMapping("/findByExecuteDocinternal/{docId}")
	public ResponseEntity<?> findByExecuteDocinternal(@PathVariable Long docId) {
		return new ResponseEntity<>(docInternalService.findByExecuteDocinternal(docId), HttpStatus.OK);
	}
}
