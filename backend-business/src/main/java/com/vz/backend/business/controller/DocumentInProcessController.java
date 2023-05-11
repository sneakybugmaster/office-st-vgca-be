package com.vz.backend.business.controller;

import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.config.DocumentCommentTypeEnum;
import com.vz.backend.business.domain.DocumentInProcess;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.dto.UserConditionDto;
import com.vz.backend.business.service.DocumentInProcessService;
import com.vz.backend.business.service.DocumentService;
import com.vz.backend.business.service.DocumentUserService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.controller.BaseController;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.service.IService;

import javax.websocket.server.PathParam;

@RestController
@RequestMapping("/doc_in_process")
public class DocumentInProcessController extends BaseController<DocumentInProcess> {

	@Autowired
	DocumentInProcessService processService;

	@Autowired
	DocumentUserService docUserService;

	@Autowired
	DocumentService docService;

	@Override
	public IService<DocumentInProcess> getService() {
		return processService;
	}

	@PostMapping(value = "/setImportant")
	public ResponseEntity<?> setImportant(@RequestParam Long docId, @RequestParam Boolean important) {
		return new ResponseEntity<>(docUserService.setImportant(DocumentTypeEnum.VAN_BAN_DEN, docId, important),
				HttpStatus.OK);
	}

	@GetMapping(value = "/finishReceiveToKnow")
	public ResponseEntity<?> finishReceiveToKnow(@RequestParam List<Long> docIds, @RequestParam(required = false) String comment) {
		return new ResponseEntity<>(processService.finishReceiveToKnow(docIds, comment), HttpStatus.OK);
	}
	
	@PostMapping(value = "/done/{docId}")
	public ResponseEntity<?> done(@PathVariable Long docId, @RequestParam(value = "comment", required = false) String comment,
			@RequestParam(value = "files", required = false) MultipartFile[] files,
			@RequestParam(value = "signDate", required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) Date signDate,
			@RequestParam(required = false) Boolean special) {
		if (Boolean.TRUE.equals(special)) { // hoàn thành vb chuyển đơn vị
			return new ResponseEntity<>(docService.finish(docId, comment, files), HttpStatus.OK);
		}
		docService.validDocId(docId);
		return new ResponseEntity<>(processService.done(docId, comment, files, signDate), HttpStatus.OK);
	}

	@GetMapping(value = "/checkTypeHandleByDoc/{docId}")
	public ResponseEntity<ListObjectDto<DocumentInProcess>> checkTypeHandleByDoc(@PathVariable Long docId) {
		docService.validDocId(docId);
		return new ResponseEntity<>(processService.checkTypeHandleByDoc(docId), HttpStatus.OK);
	}
	
	@GetMapping(value = "/getTypeHandleByUsername/{docId}")
	public ResponseEntity<?> getTypeHandleByUsername(@PathVariable Long docId) {
		Documents doc = docService.validDocId(docId);
		return new ResponseEntity<>(processService.getActionByDoc(doc), HttpStatus.OK);
	}

	@GetMapping(value = "/listTracking/{docId}")
	public ResponseEntity<?> listTracking(@PathVariable Long docId) {
		docService.validDocId(docId);
		return new ResponseEntity<>(processService.listTracking(docId), HttpStatus.OK);
	}

	@PostMapping(value = "/delegate_finish/{docId}")
	public ResponseEntity<?> delegateFinish(@PathVariable Long docId, @RequestParam(value = "comment") String comment,
			@RequestParam(required = false) String tab, @RequestParam(required = false) boolean special,
			@RequestParam(value = "files", required = false) MultipartFile[] files) {
		if (Boolean.TRUE.equals(special)) {
			return new ResponseEntity<>(docService.finish(docId, comment, files), HttpStatus.OK);
		}
		docService.validDocId(docId);
		tab = BussinessCommon.convert(tab);
		return new ResponseEntity<>(processService.done(docId, comment, files, null), HttpStatus.OK);
	}

	@PostMapping(value = "/progress_report/{docId}")
	public ResponseEntity<DocumentInProcess> progressReport(@PathVariable Long docId,
			@RequestParam(required = false) Integer progress, @RequestParam(required = false) String tab,
			@RequestParam(required = false) String comment) {
		docService.validDocId(docId);
		tab = BussinessCommon.convert(tab);
		return new ResponseEntity<>(processService.progressReport(docId, progress, comment, tab), HttpStatus.OK);
	}
	
	@GetMapping("/users/{nodeId}/{step}/{docId}")
	public ResponseEntity<List<UserConditionDto>> getNodeUsers(@PathVariable Long nodeId, @PathVariable Long docId, @PathVariable int step) {
		List<UserConditionDto> users = processService.getUserByNodeId(docId, null, step);
		return new ResponseEntity<>(users, HttpStatus.OK);
	}
	
	@GetMapping("/users/{nodeId}/{docId}")
	public ResponseEntity<List<UserConditionDto>> getNodeUsers(@PathVariable Long nodeId, @PathVariable Long docId) {
		List<UserConditionDto> users = processService.getUserByNodeId(docId, null);
		return new ResponseEntity<>(users, HttpStatus.OK);
	}

	@GetMapping(value = "/tracking/list/{docId}")
	public ResponseEntity<?> tracking(@PathVariable Long docId) {
		return new ResponseEntity<>(processService.buildTree(docId), HttpStatus.OK);
	}
	
	/**
	 * get organization that transfered
	 * @param docIds
	 * @return
	 */
	@GetMapping(value = "/org/{docId}")
	public ResponseEntity<?> getTransferedOrg(@PathVariable Long docId) {
		return new ResponseEntity<>(docService.getTransferedOrg(docId), HttpStatus.OK);
	}
	
	/**
	 * #2803 : Hoàn thành đối với văn bản đến nội bộ
	 * @param docId
	 * @param comment
	 * @param files
	 * @param docIds
	 * @return
	 */
	@PostMapping(value = "/done")
	public ResponseEntity<?> done(@RequestParam(required = false) String comment,
			@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam List<Long> docIds) {
		return new ResponseEntity<>(processService.done(docIds, comment, files), HttpStatus.OK);
	}
}
