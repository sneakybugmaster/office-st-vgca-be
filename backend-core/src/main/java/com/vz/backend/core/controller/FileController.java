package com.vz.backend.core.controller;

import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import javax.websocket.server.PathParam;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.mvc.method.annotation.MvcUriComponentsBuilder;

import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.FileInfo;
import com.vz.backend.core.domain.ResponseMessage;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.FileService;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.core.service.UserService;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/files")
public class FileController {
	@Autowired
	FilesStorageService storageService;

	@Autowired
	UserService userService;
	
	@Autowired
	FileService fileService;
	
	@PostMapping("/tmp")
	public ResponseEntity<ResponseMessage> tmp(@RequestParam(name = "file", required = false) MultipartFile file) {
		return new ResponseEntity<>(new ResponseMessage("suscess"), HttpStatus.OK);
	}

	@PostMapping("/upload")
	public ResponseEntity<ResponseMessage> uploadFile(@RequestParam("file") MultipartFile file) {
		try {
			String newFileName = storageService.save(file);
			return new ResponseEntity<>(new ResponseMessage(newFileName), HttpStatus.OK);
		} catch (Exception e) {
			e.printStackTrace();
			return new ResponseEntity<>(HttpStatus.EXPECTATION_FAILED);
		}
	}

	private static final Pattern pattern = Pattern.compile("(\\.)([^\\\\.\\\\s]*[^\\_])(_)");

	@GetMapping("/avatar/{fileName:.+}")
	public ResponseEntity<?> loadAvartar(@PathVariable String fileName) {
		Matcher m = pattern.matcher(fileName);
		if (!m.find()) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		String type = m.group(2);

		Resource file = storageService.load(StringUtils.decodeFromUrl(fileName));
		if (file == null) {
			file = new UrlResource(getClass().getClassLoader().getResource("no-photo.jpg"));
		}
		return ResponseEntity.ok().header(HttpHeaders.CONTENT_TYPE, "image/" + type).body(file);
	}

	@PostMapping("/upload-sign-file/vgca")
	public ResponseEntity<ResponseMessage> uploadSignFileVGCA(@RequestParam("uploadfile") MultipartFile uploadfile) {
		try {
			storageService.saveToSystem(uploadfile, FilesStorageService.getFileName(uploadfile));
			return ResponseEntity.status(HttpStatus.OK).build();
		} catch (Exception e) {
			e.printStackTrace();
			return new ResponseEntity<>(HttpStatus.EXPECTATION_FAILED);
		}
	}
	@PostMapping("/upload-sign-file")
	public ResponseEntity<ResponseMessage> uploadSignFile(@RequestParam("file") MultipartFile uploadfile) {
		try {
			storageService.saveToSystem(uploadfile, FilesStorageService.getFileName(uploadfile));
			return ResponseEntity.status(HttpStatus.OK).build();
		} catch (Exception e) {
			e.printStackTrace();
			return new ResponseEntity<>(HttpStatus.EXPECTATION_FAILED);
		}
	}

	@GetMapping("/getAll")
	public ResponseEntity<List<FileInfo>> getListFiles() {
		List<FileInfo> fileInfos = storageService.loadAll().map(path -> {
			String filename = path.getFileName().toString();
			String url = MvcUriComponentsBuilder
					.fromMethodName(FileController.class, "getFile", path.getFileName().toString()).build().toString();

			return new FileInfo(filename, url);
		}).collect(Collectors.toList());

		return ResponseEntity.status(HttpStatus.OK).body(fileInfos);
	}

	@GetMapping("/delete")
	public ResponseEntity<?> deleteFileName(@PathParam(value = "fileName") String fileName) {
		if (fileName == null || fileName.length() == 0) {
			return ResponseEntity.status(HttpStatus.EXPECTATION_FAILED).build();
		}

		boolean success = storageService.deleteFile(fileName);
		if (success) {
			return ResponseEntity.status(HttpStatus.OK).build();
		}

		return ResponseEntity.status(HttpStatus.EXPECTATION_FAILED).build();
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
	public ResponseEntity<Resource> getTmpFile(@PathVariable String fileName) {
		Pattern pattern = Pattern.compile("(\\.)([^\\\\.\\\\s]*[^\\_])(_)");
		Matcher m = pattern.matcher(fileName);
		if (!m.find()) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}

		Resource file = storageService.loadTmpFile(StringUtils.decodeFromUrl(fileName));
		if (file == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + file.getFilename() + "\"")
				.body(file);
	}
	
	@GetMapping("/detete-tmp-file/{fileName:.+}")
	public ResponseEntity<?> deleteTmpFile(@PathVariable String fileName) {
		storageService.deleteTmpFile(StringUtils.decodeFromUrl(fileName));
		return ResponseEntity.status(HttpStatus.OK).build();
	}

	@PostMapping("/page-size")
	public ResponseEntity<?> getPageSize(@RequestParam("file") MultipartFile file) {
		return new ResponseEntity<>(fileService.getPageSize(file), HttpStatus.OK);
	}
}
