package com.vz.backend.business.controller;

import java.util.List;

import com.vz.backend.core.util.StreamUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.servlet.mvc.method.annotation.StreamingResponseBody;

import com.vz.backend.business.config.HsRecordEnum;
import com.vz.backend.business.domain.hstl.HsFolderRecord;
import com.vz.backend.business.domain.hstl.HsFolderRecordForm;
import com.vz.backend.business.dto.hstl.ecm.FolderStatusListDto;
import com.vz.backend.business.dto.hstl.ecm.FormRegisterDto;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordFormListDto;
import com.vz.backend.business.dto.hstl.ecm.HsFolderRecordListDto;
import com.vz.backend.business.dto.hstl.ecm.UpdateStatusRegisterFormDto;
import com.vz.backend.business.service.hstl.HsFolderRecordFormService;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.dto.LabelValueDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/hstl-form")
public class HsFolderRecordFormController {

	@Autowired
	private HsFolderRecordFormService formService;
	
	@Autowired
	private FilesStorageService storageService;

	@GetMapping("/register/{formId}")
	public ResponseEntity<HsFolderRecordForm> register(@PathVariable Long formId) {
		HsFolderRecordForm input = formService.valid(formId, Message.NOT_FOUND_OBJECT);
		formService.register(input, false);
		return new ResponseEntity<>(input, HttpStatus.OK);
	}
	
	@GetMapping("/{formId}")
	public ResponseEntity<HsFolderRecordForm> get(@PathVariable Long formId) {
		return new ResponseEntity<>(formService.getDetail(formId), HttpStatus.OK);
	}
	
	@PostMapping("/add")
	public ResponseEntity<HsFolderRecordForm> add(@RequestBody HsFolderRecordForm input) {
		input = formService.save(input);
		return new ResponseEntity<>(input, HttpStatus.OK);
	}

	@PostMapping("/form/update/{id}")
	public ResponseEntity<HsFolderRecordForm> update(@RequestBody HsFolderRecordForm input, @PathVariable Long id) {
		return new ResponseEntity<>(formService.update(input, id), HttpStatus.OK);
	}
	
	@PostMapping("/update/{id}")
	public ResponseEntity<HsFolderRecordForm> updateStatusRegisterForm(@PathVariable Long id, @RequestBody UpdateStatusRegisterFormDto dto) {
		return new ResponseEntity<>(formService.updateStatusRegisterForm(id, dto), HttpStatus.OK);
	}
	
	@GetMapping("/delete/{formId}")
	public ResponseEntity<Boolean> delete(@PathVariable Long formId) {
		return new ResponseEntity<>(formService.delForm(formId), HttpStatus.OK);
	}

	@GetMapping("/list")
	public ResponseEntity<Page<HsFolderRecordFormListDto>> list(
			@RequestParam(required = false, defaultValue = "1") Integer page,
			@RequestParam(required = false, defaultValue = "FALSE") Boolean register) {
		return new ResponseEntity<>(formService.list(register, page), HttpStatus.OK);
	}
	
	@GetMapping("/all")
	public ResponseEntity<List<FormRegisterDto>> all() {
		return new ResponseEntity<>(formService.all(), HttpStatus.OK);
	}

	@PostMapping("/export")
	public ResponseEntity<StreamingResponseBody> exportDocs(@RequestBody HsFolderRecordForm input) {
		StreamingResponseBody stream = outputStream -> {
			try {
				formService.export(outputStream, input);
			} finally {
				StreamUtils.closeOutputStream(outputStream);
			}
		};
		return new ResponseEntity<>(stream, HttpStatus.OK);
	}

	@PostMapping("/folders/register/{formId}")
	public ResponseEntity<?> register(@PathVariable Long formId, @RequestParam List<Long> ids) {
		List<HsFolderRecord> rs = formService.saveFolderRecords(formId, ids);
		formService.register(formId, rs);
		return new ResponseEntity<>(rs, HttpStatus.OK);
	}

	/**
	 * Update list folder corresponding to status
	 * @param dto
	 * @return
	 */
	@PostMapping("/folders/update/{formId}")
	public ResponseEntity<List<HsFolderRecord>> updateFolderRecords(@PathVariable Long formId, @RequestBody FolderStatusListDto dto) {
		return new ResponseEntity<>(formService.updateFolderRecords(formId, dto), HttpStatus.OK);
	}

	@GetMapping("/folders/list")
	public ResponseEntity<Page<HsFolderRecordListDto>> getListFolderRecord(
			@RequestParam(required = false, defaultValue = "1") Integer page,
			@RequestParam(required = false, defaultValue = "FALSE") Boolean register) {
		return new ResponseEntity<>(formService.getListFolderRecord(register, page), HttpStatus.OK);
	}
	
	/**
	 * Download file with type : 
	 * type = 1: hs folder file
	 * type = 2 : task attachments of form/document in att/ document out att
	 * @param type
	 * @param name
	 * @return
	 */
	@GetMapping("/download/{type}/{name:.+}")
	public ResponseEntity<Resource> download(@PathVariable int type, @PathVariable String name) {
		if (type < 1 || type > 4)
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);

		name = StringUtils.decodeFromUrl(name);
		Resource file = type == 1 ? storageService.loadHs(name) : storageService.load(name);
		if (file == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + file.getFilename() + "\"")
				.body(file);
	}
	
	@GetMapping("/enum/{type}")
	public ResponseEntity<List<LabelValueDto<String>>> getEnum(@PathVariable String type) {
		return new ResponseEntity<>(HsRecordEnum.TRUC_TIEP.get(type), HttpStatus.OK);
	}
	
	/**
	 * For testing
	 * @return
	 */
	@GetMapping("/save-file")
	public ResponseEntity<?> saveFile() {
		return new ResponseEntity<>(formService.savefile(), HttpStatus.OK);
	}
}
