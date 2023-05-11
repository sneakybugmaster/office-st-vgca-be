package com.vz.backend.business.controller;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;
import java.util.Arrays;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.google.common.net.HttpHeaders;
import com.vz.backend.business.domain.AttachmentVersion;
import com.vz.backend.business.domain.DocumentOutAttachment;
import com.vz.backend.business.repository.IAttachmentVersionRepository;
import com.vz.backend.business.service.DocumentOutAttachmentService;
import com.vz.backend.business.service.DocumentOutCommentService;
import com.vz.backend.business.service.DocumentOutProcessService;
import com.vz.backend.business.service.DocumentOutService;
import com.vz.backend.business.service.DocumentOutTrackingService;
import com.vz.backend.core.auth.TokenHelper;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentOutTrackingEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.dto.ResponseUploadSignFile;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.core.service.IService;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/doc_out_attach")
public class DocumentOutAttachmentController {
	@Autowired
	private DocumentOutAttachmentService docOutAttachService;

	@Autowired
	private DocumentOutProcessService docOutProcessService;

	@Autowired
	private DocumentOutService docOutService;

	@Autowired
	private FilesStorageService storageService;

	@Autowired
	private DocumentOutCommentService docOutCmtService;

	@Autowired
	private IAttachmentVersionRepository attachVersionRepository;
	
	@Autowired
	private DocumentOutTrackingService docOutTrackingService;
	
	@Autowired
	private EncryptionService encryptService;

	public IService<DocumentOutAttachment> getService() {
		return docOutAttachService;
	}

	@PostMapping(value = "/add/{attType}/{objId}")
	public ResponseEntity<?> addAttachment(@PathParam("files") MultipartFile[] files, @PathVariable String attType,
			@PathVariable String objId) {
		Long objIdL = BussinessCommon.castId(objId);
		if (objIdL == null) {
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
		}
		return new ResponseEntity<>(docOutAttachService.addListAttachment(files, attType, objIdL), HttpStatus.OK);
	}

	@PostMapping(value = "/edit/{attType}/{objId}")
	public ResponseEntity<?> editAttachment(@PathParam("files") MultipartFile[] files, @PathVariable String attType,
			@PathVariable String objId) {
		Long objIdL = BussinessCommon.castId(objId);
		if (objIdL == null) {
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
		}
		return new ResponseEntity<>(docOutAttachService.addListAttachment(files, attType, objIdL), HttpStatus.OK);
	}

	@PostMapping("/update-sign-file/word/{attachId}")
	public ResponseEntity<ResponseUploadSignFile> updateSignWordFileVGCA(@RequestParam("uploadfile") MultipartFile file,
			@PathVariable("attachId") long attachId) {
		ResponseUploadSignFile response = updateSign(file, attachId);
		return new ResponseEntity<>(response, HttpStatus.OK);
	}
	
	@PostMapping("/update-sign-file/word/{attachId}/hsm")
	public ResponseEntity<ResponseUploadSignFile> updateSignWordFileHSM(@RequestParam("file") MultipartFile file,
			@PathVariable("attachId") long attachId) {
		ResponseUploadSignFile response = updateSign(file, attachId);
		return new ResponseEntity<>(response, HttpStatus.OK);
	}

	@PostMapping("/update-sign-file/pdf/{attachId}")
	public ResponseEntity<ResponseUploadSignFile> updateSignPDFFileVGCA(@RequestParam("uploadfile") MultipartFile file,
			@PathVariable("attachId") long attachId) {
		ResponseUploadSignFile response = updateSign(file, attachId);
		return new ResponseEntity<>(response, HttpStatus.OK);
	}
	
	@PostMapping("/update-sign-file/pdf/{attachId}/hsm")
	public ResponseEntity<ResponseUploadSignFile> updateSignPDFFileHSM(@RequestParam("file") MultipartFile file,
			@PathVariable("attachId") long attachId) {
		ResponseUploadSignFile response = updateSign(file, attachId);
		return new ResponseEntity<>(response, HttpStatus.OK);
	}
	
	public ResponseUploadSignFile updateSign(MultipartFile file, long attachId) {
		ResponseUploadSignFile response = new ResponseUploadSignFile(true, "Successed", file.getOriginalFilename(), "");
		TokenHelper tokenHelper = new TokenHelper();

		if (file == null) {
			response.setStatus(false);
			response.setMessage("Failed");
			return response;
		}
		Optional<DocumentOutAttachment> oDocAttach = docOutAttachService.findById(attachId);
		if (!oDocAttach.isPresent()) {
			response.setStatus(false);
			response.setMessage("Failed");
			return response;
		} else {
			response.setFileServer(oDocAttach.get().getName());
		}

		docOutAttachService.updateSignFile(oDocAttach.get(), file);
		return response;
	}

	@PostMapping("/edit-file/{attachId}")
	public ResponseEntity<DocumentOutAttachment> editFile(@RequestParam("file") MultipartFile file,
			@PathVariable("attachId") long attachId) {
		if (file == null) {
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		Optional<DocumentOutAttachment> oDocAttach = docOutAttachService.findById(attachId);
		if (!oDocAttach.isPresent()) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		DocumentOutAttachment doa = docOutAttachService.editFileUpload(oDocAttach.get(), file, true);
		return new ResponseEntity<>(doa, HttpStatus.OK);
	}

	@PostMapping("/convert-file/{attachId}")
	public ResponseEntity<DocumentOutAttachment> convertFile(@RequestParam("file") MultipartFile file,
			@PathVariable("attachId") long attachId) {
		if (file == null) {
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		Optional<DocumentOutAttachment> oDocAttach = docOutAttachService.findById(attachId);
		if (!oDocAttach.isPresent()) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}

		DocumentOutAttachment doa = docOutAttachService.editFile(oDocAttach.get(), file, false);
		return new ResponseEntity<>(doa, HttpStatus.OK);
	}

	@PostMapping("/updateFile/{id}")
	public ResponseEntity<DocumentOutAttachment> updateFile(@PathVariable Long id,
			@PathParam("file") MultipartFile file) {
		if (file == null) {
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}
		Optional<DocumentOutAttachment> oDocAttach = docOutAttachService.findById(id);
		if (!oDocAttach.isPresent()) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		
		Long docId = oDocAttach.get().getDocId();
		// Validate quyền update văn bản của user
		if (!docOutService.checkStatusInList(docId,
				Arrays.asList(DocumentStatusEnum.DU_THAO, DocumentStatusEnum.BI_TRA_LAI))
				&& !docOutProcessService.isNoProcess(docId)) {
			throw new RestExceptionHandler("Trạng thái văn bản không cho phép xóa đính kèm");
		}
		DocumentOutAttachment doa = docOutAttachService.updateFile(oDocAttach.get(), file);
		return new ResponseEntity<>(doa, HttpStatus.OK);
	}

	@PostMapping(value = "/deleteById/{id}")
	public ResponseEntity<?> deleteAttachmentsById(@PathVariable Long id, @RequestParam(required = false, defaultValue = "FALSE") Boolean force) {
		return new ResponseEntity<>(docOutAttachService.deleteAttachmentsById(id, force), HttpStatus.OK);
	}

	@GetMapping("/download/{fileName:.+}")
//	@ResponseBody
	public ResponseEntity<StreamingResponseBody> getFile(@PathVariable String fileName, HttpServletResponse response) throws IOException {
		String tmpName = StringUtils.decodeFromUrl(fileName);
		Long docId = null;
		Long commentId = null;
		DocumentOutAttachment tmp = null;
		long lengthFile = 0;
//		if (fileName.indexOf("_v") > 0) {
			Optional<AttachmentVersion> oDoa = attachVersionRepository.findByName(fileName);
			if (!oDoa.isPresent()) {
				Optional<DocumentOutAttachment> a = docOutAttachService.findByName(tmpName);
				if (!a.isPresent()) {
					throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
				}
				DocumentOutAttachment doa = a.get();
				tmp = doa;
				docId = doa.getDocId();
				commentId = doa.getCmtId();

			}else{
				AttachmentVersion doa = oDoa.get();
				docId = doa.getDocId();
			}

		if (docId == null || docId < 1) {
			docId = docOutCmtService.getDocIdById(commentId);
		}
		
		if (docId != null) {
			// Validate quyền truy cập văn bản của user
			docOutService.validatePermission(docId);

			docOutTrackingService.save(docId, BussinessCommon.getUserId(), null, tmpName,
					DocumentOutTrackingEnum.DOWNLOAD_FILE);
		}

		StreamingResponseBody stream;
		String outputName;
		Resource file = null;
		if (tmp != null && Boolean.TRUE.equals(tmp.getEncrypt())) {

			outputName = FilenameUtils.removeExtension(FilesStorageService.origin(tmpName)) + ".zip";
		} else {
			file = storageService.load(tmpName);
			if (file == null) {
				throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
			}

			outputName = FilesStorageService.origin(tmpName);
			lengthFile = file.contentLength();
		}
		response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + outputName + "\"");
		if (lengthFile > 0) {
			response.setContentLengthLong(lengthFile);
		}
		if (file != null) {
			response.setContentType(MediaTypeFactory.getMediaType(file).orElse(MediaType.APPLICATION_OCTET_STREAM).toString());
			try (InputStream is = file.getInputStream();
				 OutputStream os = response.getOutputStream()) {
				StreamUtils.copy(is, os);
			}
			return ResponseEntity.ok().build();
		} else {
			response.setContentType(MediaType.APPLICATION_OCTET_STREAM_VALUE);
			try (OutputStream outputStream = response.getOutputStream()) {
				encryptService.load(outputStream, tmpName);
				outputStream.flush();
			}
			return ResponseEntity.ok().build();
		}
	}

	@GetMapping("/getListAttachmnet/{docId}")
	public ResponseEntity<?> getListAttachment(@PathVariable("docId") Long docId) {
		if (docId != null && docId > 0) {
			docOutService.validatePermission(docId);
		}
		return new ResponseEntity<>(docOutAttachService.getListAttachment(docId), HttpStatus.OK);
	}
	
	@PostMapping("/upload-tmp-file")
	public ResponseEntity<?> uploadTmpFile(@RequestParam("file") MultipartFile file) {
		if (file == null) {
			return new ResponseEntity<>(HttpStatus.BAD_REQUEST);
		}

		String fileName = FilesStorageService.getFileName(file);
		storageService.saveTmpFile(file, fileName);
		
		return ResponseEntity.status(HttpStatus.OK).build();
	}
	
	@GetMapping("/download-tmp-file/{fileName:.+}")
	@ResponseBody
	public ResponseEntity<Resource> getTmpFile(@PathVariable String fileName, HttpServletResponse response) throws IOException {
		Pattern pattern = Pattern.compile("(\\.)([^\\\\.\\\\s]*[^\\_])(_)");
		Matcher m = pattern.matcher(fileName);
		if (!m.find()) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		long lengthFile = 0;
		Resource file = storageService.loadTmpFile(StringUtils.decodeFromUrl(fileName));
		if (file == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		String outputName = FilesStorageService.origin(fileName);
		lengthFile = file.contentLength();
		response.setHeader(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + outputName + "\"");
		if (lengthFile > 0) {
			response.setContentLengthLong(lengthFile);
		}
		response.setContentType(MediaTypeFactory.getMediaType(file).orElse(MediaType.APPLICATION_OCTET_STREAM).toString());
		try (InputStream is = file.getInputStream();
			OutputStream os = response.getOutputStream()) {
			StreamUtils.copy(is, os);
			return ResponseEntity.ok().build();
		}
	}
	
	@GetMapping("/detete-tmp-file/{fileName:.+}")
	public ResponseEntity<?> deleteTmpFile(@PathVariable String fileName) {
		storageService.deleteTmpFile(StringUtils.decodeFromUrl(fileName));
		return ResponseEntity.status(HttpStatus.OK).build();
	}
}
