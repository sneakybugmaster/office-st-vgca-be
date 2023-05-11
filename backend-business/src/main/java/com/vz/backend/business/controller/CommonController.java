
package com.vz.backend.business.controller;

import com.vz.backend.business.config.DocumentCommentTypeEnum;
import com.vz.backend.business.config.ViewStatusEnum;
import com.vz.backend.business.dto.EncryptDto;
import com.vz.backend.business.dto.FileNameDto;
import com.vz.backend.business.dto.ResultQuickSearchDto;
import com.vz.backend.business.dto.report.ReportData;
import com.vz.backend.business.service.CommonService;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.domain.Encryption;
import com.vz.backend.core.domain.Module;
import com.vz.backend.core.dto.EncryptionDto;
import com.vz.backend.core.dto.EncryptionFieldDto;
import com.vz.backend.core.dto.LabelValueId;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.service.EncryptionService;
import com.vz.backend.util.DateTimeUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.DateTimeFormat.ISO;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Date;
import java.util.List;

@RestController
@RequestMapping("/common")
public class CommonController {

	@Autowired
	private CommonService commonService;

	@Autowired
	private EncryptionService encryptService;

	@GetMapping("/quick-search/{page}")
	public ResponseEntity<ListObjectDto<ResultQuickSearchDto>> quickSearchAllObject(@PathVariable int page,
			@RequestParam(required = false) String text) {
		return new ResponseEntity<>(commonService.quickSearchAllObject(page, text), HttpStatus.OK);
	}

	@GetMapping("/quick-search")
	public ResponseEntity<List<ResultQuickSearchDto>> quickSearchAllObject(
			@RequestParam(required = false) String text) {
		return new ResponseEntity<>(commonService.quickSearchAllObject(text), HttpStatus.OK);
	}
	
	@GetMapping("/statistic")
	public ResponseEntity<ReportData> statistic(@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date from,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date to) {
		from = DateTimeUtils.handleSubmit(from);
		to = DateTimeUtils.getEndDate(to);
//		if (from == null && to == null) {
//			int curQuater = LocalDate.now().get(IsoFields.QUARTER_OF_YEAR);
//			int curYear = LocalDate.now().getYear();
//			from = DateTimeUtils.firstDayOfQuarter(curQuater, curYear);
//			to = DateTimeUtils.lastDateOfQuater(curQuater, curYear);
//		}
		return new ResponseEntity<>(commonService.statistic(from, to), HttpStatus.OK);
	}

	@GetMapping("/statistic-by-view-status")
	public ResponseEntity<?> statisticByViewStatus(@RequestParam(value = "orgId", required = false) Long orgId,
												   @RequestParam(value = "handleType", required = false) HandleTypeEnum handleType,
												   @RequestParam(value = "startDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date startDate,
												   @RequestParam(value = "endDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date endDate) {
		return ResponseEntity.ok(commonService.statisticByViewStatus(orgId, handleType, startDate, endDate));
	}

	@GetMapping("/doc-list-by-view-status")
	public ResponseEntity<?> documentListByViewStatus(@RequestParam(value = "page", defaultValue = "0") int page,
													  @RequestParam(value = "size", defaultValue = "10") int size,
													  @RequestParam(value = "orgId", required = false) Long orgId,
													  @RequestParam(value = "viewStatus") ViewStatusEnum viewStatus,
													  @RequestParam(value = "handleType", required = false) HandleTypeEnum handleType,
													  @RequestParam(value = "startDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date startDate,
													  @RequestParam(value = "endDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date endDate
	) {
		Pageable pageable = PageRequest.of(page, size);
		return ResponseEntity.ok(commonService.documentsByViewStatus(orgId, viewStatus, handleType, startDate, endDate, pageable));
	}

	@GetMapping("/StactisticsNotComplete")
	public ResponseEntity<?> statistic() {
		return new ResponseEntity<>(commonService.StactisticsNotComplete(), HttpStatus.OK);
	}

	@GetMapping("/usage")
	public ResponseEntity<List<LabelValueId<String>>> usage() {
		return new ResponseEntity<>(commonService.usage(), HttpStatus.OK);
	}
	
	@PostMapping("/usage/edit/{id}")
	public ResponseEntity<Module> editUsageModule(@PathVariable Long id, @RequestParam(required = false) String name, @RequestParam(required = false) MultipartFile file) {
		return new ResponseEntity<>(commonService.editUsageModule(id, name, file), HttpStatus.OK);
	}
	
	@PostMapping("/usage/del/{id}")
	public ResponseEntity<Boolean> delUsageModule(@PathVariable Long id ) {
		return new ResponseEntity<>(commonService.delUsageModule(id), HttpStatus.OK);
	}

	@GetMapping("/quick-search/key")
	public ResponseEntity<List<String>> getKeySearch() {
		return new ResponseEntity<>(commonService.getKeySearch(), HttpStatus.OK);
	}

	@PostMapping("/encrypt")
	public ResponseEntity<EncryptDto> encrypt(@RequestParam String key, @RequestParam MultipartFile encrypted,
											  @RequestParam Long objId, @RequestParam DocumentTypeEnum type) {
		return new ResponseEntity<>(commonService.encryptAndFileName(key, encrypted, objId, type), HttpStatus.OK);
	}

	@PostMapping("/encrypt/add")
	public ResponseEntity<List<Encryption>> addMultipleEncrypts(@RequestBody EncryptionDto data) {
		List<Encryption> encryptions = data.getData();
		return new ResponseEntity<>(encryptService.save(encryptions), HttpStatus.OK);
	}

	/**
	 * Lấy cert theo người dùng/ tổ chức đối với màn hình có show được người chọn
	 * @param userIds
	 * @param orgIds
	 * @param type
	 * @param skipError
	 * @return
	 */
	@GetMapping("/user/cert")
	public ResponseEntity<List<LabelValueId<String>>> getCertByUserId(@RequestParam(required = false) Long[] userIds,
			@RequestParam(required = false) Long[] orgIds, @RequestParam(required = false) String type,
			@RequestParam(required = false) Boolean skipError) {
		return new ResponseEntity<>(commonService.getCertByUserId(userIds, orgIds, type, skipError), HttpStatus.OK);
	}

	/**
	 * Lấy cert theo người dùng/ tổ chức đối với màn hình có show được người chọn chỉ check trường phòng
	 * @param userIds
	 * @param orgIds
	 * @param type
	 * @param skipError
	 * @return
	 */
	@GetMapping("/user/cert-manager")
	public ResponseEntity<List<LabelValueId<String>>> getCertByUserIdManager(@RequestParam(required = false) Long[] userIds,
			@RequestParam(required = false) Long[] orgIds, @RequestParam(required = false) String type,
			@RequestParam(required = false) Boolean skipError) {
		return new ResponseEntity<>(commonService.getCertByUserIdManager(userIds, orgIds, type, skipError), HttpStatus.OK);
	}
	
	/**
	 * Lấy cert theo đối tượng đối với màn hình không show danh sách người chọn
	 * @param objId
	 * @param type
	 * @return
	 */
	@GetMapping("/user/cert/{objId}")
	public ResponseEntity<List<LabelValueId<String>>> getCertByObjId(@PathVariable(required = false) Long objId,
			@RequestParam(required = false) String type) {
		return new ResponseEntity<>(commonService.getCertByObjId(objId, type), HttpStatus.OK);
	}

	@GetMapping("/user/cert/edit/{objId}")
	public ResponseEntity<List<LabelValueId<String>>> getAllRelateByDocIdEdit(@PathVariable(required = false) Long objId,
																	 @RequestParam(required = false) String type) {
		return new ResponseEntity<>(commonService.getCertByObjIdEdit(objId, type), HttpStatus.OK);
	}

	@GetMapping("/user/file-id")
	public ResponseEntity<List<EncryptionFieldDto>> getFileId(@RequestParam(required = false) String[] fileNameList) {
		return new ResponseEntity<>(encryptService.getFileId(fileNameList), HttpStatus.OK);
	}

	@GetMapping("/user/share-file/{type}/{objId}")
	public ResponseEntity<List<LabelValueId<String>>> getUserShareFile(@PathVariable String type, @PathVariable Long objId) {
		return new ResponseEntity<>(commonService.getUserShareFile(objId, type), HttpStatus.OK);
	}
	
	@GetMapping("/user/shared-file")
	public ResponseEntity<List<Long>> getUserSharedFile(@RequestParam String fileNames) {
		return new ResponseEntity<>(commonService.getUserSharedFile(fileNames), HttpStatus.OK);
	}
	
	@PostMapping("/comment/add/{objId}")
	public ResponseEntity<FileNameDto> saveCommentByType(@PathVariable Long objId, @RequestParam String comment,
			@RequestParam(required = false) String cmtContent, @RequestParam MultipartFile[] encFiles,
			@RequestParam MultipartFile[] nonEncfiles, @RequestParam DocumentTypeEnum type,
			@RequestParam(required = false) DocumentCommentTypeEnum cmtType, @RequestParam(required = false) String[] keys) {
		return new ResponseEntity<>(
				commonService.saveCommentByType(objId, type, comment, cmtContent, nonEncfiles, encFiles, keys, cmtType),
				HttpStatus.OK);
	}

	@PostMapping("/comment/del")
	public ResponseEntity<Boolean> delCommentByType(@RequestParam(required = false) String[] fileNames,
			@RequestParam(required = false) Long[] userIds, @RequestParam DocumentTypeEnum type,
			@RequestParam(required = false) Long cmtIdSaved) {
		return new ResponseEntity<>(commonService.delCommentByType(fileNames, userIds, type, cmtIdSaved),
				HttpStatus.OK);
	}
	
	@GetMapping("/attach/list/{type}/{objId}")
	public ResponseEntity<List<String>> getAttachsByTypeAndObjId(@PathVariable Long objId, @PathVariable String type) {
		return new ResponseEntity<>(commonService.getAttachsByTypeAndObjId(objId, type), HttpStatus.OK);
	}

	@GetMapping("/user/lead")
	public ResponseEntity<?> getUserLeadOrg() {
		return new ResponseEntity<>(commonService.getUserIdLeadBan(), HttpStatus.OK);
	}
}
