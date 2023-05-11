package com.vz.backend.business.controller;

import java.util.LinkedHashMap;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
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

import com.vz.backend.business.domain.outsideconnect.TrackingConnectOutside;
import com.vz.backend.business.domain.outsideconnect.TrackingObjectOutside;
import com.vz.backend.business.dto.outsideconnect.DataRawRequestOutside;
import com.vz.backend.business.dto.outsideconnect.DataRequestOutside;
import com.vz.backend.business.dto.outsideconnect.OutsideSystemDto;
import com.vz.backend.business.dto.outsideconnect.RejectDataDto;
import com.vz.backend.business.dto.outsideconnect.ResultMsg;
import com.vz.backend.business.dto.outsideconnect.SentDataDto;
import com.vz.backend.business.service.OutsideConnectService;
import com.vz.backend.business.service.outsideconnect.TrackingConnectOutsideService;
import com.vz.backend.business.service.outsideconnect.TrackingObjectOutsideService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.Encryption;
import com.vz.backend.core.domain.OutsideSystem;
import com.vz.backend.core.domain.Token;
import com.vz.backend.core.dto.EncryptionDto;
import com.vz.backend.core.dto.LabelValueId;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/integrate")
public class OutsideConnectController {

	@Autowired
	private OutsideConnectService outsideConnectService;

	@Autowired
	private FilesStorageService storageService;

	@Autowired
	private TrackingObjectOutsideService trackingObjectService;

	@Autowired
	private TrackingConnectOutsideService trackingConnectService;

	@PostMapping("/rq-connect")
	public ResponseEntity<Boolean> requestConnect(@RequestBody OutsideSystem data) {
		return new ResponseEntity<>(outsideConnectService.requestConnect(data), HttpStatus.OK);
	}

	@PostMapping("/rs-connect")
	public ResponseEntity<Token> responseConnect(@RequestBody OutsideSystem data) {
		return new ResponseEntity<>(outsideConnectService.responseConnect(data), HttpStatus.OK);
	}

	@GetMapping("/rq-org/{outsideId}")
	public ResponseEntity<List<LinkedHashMap<String, String>>> getAllByGlobal(@PathVariable Long outsideId) {
		return new ResponseEntity<>(outsideConnectService.getAllByGlobal(outsideId), HttpStatus.OK);
	}
	
	@PostMapping("/rs-org")
	public ResponseEntity<List<SentDataDto>> getAllByGlobal(@RequestBody OutsideSystem data) {
		return new ResponseEntity<>(outsideConnectService.getAllByGlobal(data), HttpStatus.OK);
	}

	@PostMapping("/send-obj")
	public ResponseEntity<ResultMsg> sendObj(@RequestBody DataRawRequestOutside data) {
		return new ResponseEntity<>(outsideConnectService.sendObjList(data), HttpStatus.OK);
	}

	@PostMapping("/receive-obj")
	public ResponseEntity<?> receiveObj(@RequestBody DataRequestOutside data) {
		return new ResponseEntity<>(outsideConnectService.receiveObj(data), HttpStatus.OK);
	}
	
	@GetMapping("/obj/rq-reject/{docId}")
	public ResponseEntity<?> rejectObj(@PathVariable Long docId) {
		return new ResponseEntity<>(outsideConnectService.rejectObj(docId), HttpStatus.OK);
	}
	
	@PostMapping("/obj/rs-reject")
	public ResponseEntity<?> rejectObj1(@RequestBody RejectDataDto data) {
		return new ResponseEntity<>(outsideConnectService.rejectObj(data), HttpStatus.OK);
	}

	@GetMapping("/download/{name:.+}")
	public ResponseEntity<Resource> download(@PathVariable String name) {
		name = StringUtils.decodeFromUrl(name);
		Resource file = storageService.load(name);
		if (file == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + file.getFilename() + "\"")
				.body(file);
	}

	@GetMapping("/track/{type}/{objId}")
	public ResponseEntity<Page<TrackingObjectOutside>> getTrackingObject(@PathVariable Long objId,
			@PathVariable DocumentTypeEnum type, @RequestParam(required = false, defaultValue = "1") Integer page) {
		Pageable pageable = BussinessCommon.castToPageable(page);
		return new ResponseEntity<>(trackingObjectService.list(objId, type, pageable), HttpStatus.OK);
	}

	@GetMapping("/track/sys")
	public ResponseEntity<Page<TrackingConnectOutside>> getTrackingConnect(
			@RequestParam(required = false, defaultValue = "1") Integer page) {
		Pageable pageable = BussinessCommon.castToPageable(page);
		return new ResponseEntity<>(trackingConnectService.list(pageable), HttpStatus.OK);
	}
	
	@GetMapping("/sent/{type}/{objId}")
	public ResponseEntity<List<SentDataDto>> getSentData(@PathVariable Long objId, @PathVariable DocumentTypeEnum type) {
		return new ResponseEntity<>(trackingObjectService.getSentData(objId, type), HttpStatus.OK);
	}
	
	@GetMapping("/cert/rq-user/{outsideId}")
	public ResponseEntity<List<LinkedHashMap<String, String>>> getCertByUserId(
			@RequestParam(required = false) Long[] userIds, @PathVariable Long outsideId) {
		return new ResponseEntity<>(outsideConnectService.getCertByUserId(outsideId, userIds), HttpStatus.OK);
	}
	
	@PostMapping("/cert/rs-user")
	public ResponseEntity<List<LabelValueId<String>>> getCertByUserId(@RequestBody OutsideSystemDto data) {
		return new ResponseEntity<>(outsideConnectService.getCertByUserId(data), HttpStatus.OK);
	}
	
	@PostMapping("/encrypt/rq-add/{outsideId}")
	public ResponseEntity<List<LinkedHashMap<String, String>>> addMultipleEncrypts(@PathVariable Long outsideId,
			@RequestBody EncryptionDto data) {
		List<Encryption> encryptions = data.getData();
		return new ResponseEntity<>(outsideConnectService.addMultipleEncrypts(outsideId, encryptions), HttpStatus.OK);
	}

	@PostMapping("/encrypt/rs-add")
	public ResponseEntity<List<Encryption>> addMultipleEncrypts(@RequestBody OutsideSystemDto data) {
		return new ResponseEntity<>(outsideConnectService.addMultipleEncrypts(data), HttpStatus.OK);
	}
}
