package com.vz.backend.business.controller;

import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.format.annotation.DateTimeFormat.ISO;
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
import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.business.config.FolderPermissionEnum;
import com.vz.backend.business.config.FolderTypeEnum;
import com.vz.backend.business.config.IconTypeEnum;
import com.vz.backend.business.controller.DocumentOutController.GroupBy;
import com.vz.backend.business.domain.hstl.HsFolder;
import com.vz.backend.business.domain.hstl.HsFolderDocument;
import com.vz.backend.business.domain.hstl.HsFolderFile;
import com.vz.backend.business.domain.hstl.HsFolderShare;
import com.vz.backend.business.dto.hstl.CriteriaSearchDto;
import com.vz.backend.business.dto.hstl.DocumentByMonth;
import com.vz.backend.business.dto.hstl.FolderDataDetailDto;
import com.vz.backend.business.dto.hstl.FolderDetailDto;
import com.vz.backend.business.dto.hstl.ReportMenuDto;
import com.vz.backend.business.service.hstl.HsFolderDocumentService;
import com.vz.backend.business.service.hstl.HsFolderFileService;
import com.vz.backend.business.service.hstl.HsFolderService;
import com.vz.backend.business.service.hstl.HsFolderShareService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.ResponseMessage;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.PasswordUtils;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/hstl")
public class HsFolderController {

	private enum SortBy {
		UPDATEDATE("updateDate"),
		CREATEDATE("createDate");

		private String field;

		private SortBy(String field) {
			this.field = field;
		}
	}

	@Autowired
	private HsFolderService hsFolderService;
	@Autowired
	private HsFolderDocumentService hsFolderDocService;
	@Autowired
	private HsFolderFileService hsFolderFileService;
	@Autowired
	private FilesStorageService storageService;
	@Autowired
	private HsFolderShareService hsFolderShareService;

	@PostMapping("/addFolder")
	public ResponseEntity<?> addFolder(@RequestBody HsFolder input, @RequestParam(required = false) Long userApprove) {
		return new ResponseEntity<>(hsFolderService.add(input, userApprove), HttpStatus.OK);
	}

	@GetMapping("/approved")
	public ResponseEntity<List<HsFolder>> approved() {
		return new ResponseEntity<>(hsFolderService.approved(), HttpStatus.OK);
	}

	@GetMapping("/getById")
	public ResponseEntity<?> getById(@RequestParam Long folderId) {
		return new ResponseEntity<>(hsFolderService.getById(folderId), HttpStatus.OK);
	}

	@GetMapping("/doc/detail/{id}")
	public ResponseEntity<?> getFolderDocumentById(@PathVariable Long id) {
		HsFolderDocument doc = hsFolderDocService.valid(id, Message.NOT_FOUND_DOC);
		return new ResponseEntity<>(doc, HttpStatus.OK);
	}

	@PostMapping("/updateFolder/{id}")
	public ResponseEntity<?> updateFolder(@PathVariable Long id, @RequestBody HsFolder input, @RequestParam(required = false) Long userApprove) {
		return new ResponseEntity<>(hsFolderService.update(id, input, userApprove), HttpStatus.OK);
	}

	@PostMapping("/addDocument")
	public ResponseEntity<?> addDocument(@RequestParam(required = false) Long folderId, @RequestBody List<HsFolderDocument> input) {
		return new ResponseEntity<>(hsFolderDocService.add(folderId, input), HttpStatus.OK);
	}

	@PostMapping("/addFile")
	public ResponseEntity<ResponseMessage> addFile(@RequestParam String comment,
			@RequestParam(required = false) Long folderId,
			MultipartFile file) {
		//check permission
		if (folderId != null) {
			hsFolderService.validatePermission(folderId, FolderPermissionEnum.FULL, false);
		}
		HsFolderFile input = new HsFolderFile(folderId, comment);
		input = hsFolderFileService.add(input, file);
		return new ResponseEntity<>(new ResponseMessage(input.getId() + ""), HttpStatus.OK);
	}

	@PostMapping("/file/add")
	public ResponseEntity<ResponseMessage> addFile2(@RequestBody HsFolderFile input) {
		// check permission
		Long folderId = input.getFolderId();
		if (folderId != null) {
			hsFolderService.validatePermission(folderId, FolderPermissionEnum.FULL, false);
		}
		input = hsFolderFileService.add(input);
		return new ResponseEntity<>(new ResponseMessage(input.getId().toString()), HttpStatus.OK);
	}

	@PostMapping("/file/update/{id}")
	public ResponseEntity<ResponseMessage> addFile2(@PathVariable Long id, @RequestBody HsFolderFile input) {
		// check permission
		Long folderId = input.getFolderId();
		if (folderId != null) {
			hsFolderService.validatePermission(folderId, FolderPermissionEnum.FULL, false);
		}
		input = hsFolderFileService.update(id, input);
		return new ResponseEntity<>(new ResponseMessage(input.getId().toString()), HttpStatus.OK);
	}

	/**
	 * Add file attachment in folder file
	 * @param id
	 * @param file
	 * @return
	 */
	@PostMapping("/file/add/{id}")
	public ResponseEntity<ResponseMessage> addFileAttachment(@PathVariable Long id, @RequestParam MultipartFile file) {
		HsFolderFile input = hsFolderFileService.add(id, file);
		return new ResponseEntity<>(new ResponseMessage(input.getId().toString()), HttpStatus.OK);
	}

	@GetMapping("/file/detail/{id}")
	public ResponseEntity<?> getHsFileById(@PathVariable Long id) {
		HsFolderFile files = hsFolderFileService.valid(id, Message.NOT_FOUND_DOC);
		return new ResponseEntity<>(files, HttpStatus.OK);
	}

	@PostMapping("/addShare")
	public ResponseEntity<?> addShare(@RequestBody List<HsFolderShare> input) {
		return new ResponseEntity<>(hsFolderService.addShare(input), HttpStatus.OK);
	}
	
	@GetMapping("/stopShare/{id}")
	public ResponseEntity<?> stopShare(@PathVariable(name = "id") Long folderId) {
		return new ResponseEntity<>(hsFolderService.stopShare(folderId), HttpStatus.OK);
	}

	@GetMapping("/transferFolder")
	public ResponseEntity<?> transferFolder(@RequestParam Long folderId, @RequestParam(required = false) Long parentId) {
		return new ResponseEntity<>(hsFolderService.transferFolder(folderId, parentId), HttpStatus.OK);
	}
	
	@GetMapping("/transferFileToOtherFolder")
	public ResponseEntity<?> transferFileToOtherFolder(@RequestParam Long fileId, 
			@RequestParam Long folderIdFrom, @RequestParam Long folderIdTo, @RequestParam Long docId) {
		return new ResponseEntity<>(hsFolderService.transferFileToOtherFolder(fileId, folderIdFrom, folderIdTo, docId), HttpStatus.OK);
	}


	@GetMapping("/getListShare")
	public ResponseEntity<?> getListShare(@RequestParam Long folderId) {
		return new ResponseEntity<>(hsFolderShareService.getListShare(folderId), HttpStatus.OK);
	}
	
	@GetMapping("/download/{name:.+}")
	public ResponseEntity<Resource> download(
			@RequestParam String sign,
			@RequestParam long exp,
			@PathVariable String name) {
		String error = PasswordUtils.validSign(name, exp, sign);
		if (error != null) {
			throw new RestExceptionHandler(error);
		}

		Resource file = storageService.loadHs(StringUtils.decodeFromUrl(name));
		if (file == null) {
			throw new RestExceptionHandler(Message.NOT_FOUND_FILE);
		}
		return ResponseEntity.ok()
				.header(HttpHeaders.CONTENT_DISPOSITION, "attachment; filename=\"" + file.getFilename() + "\"")
				.body(file);
	}

	@GetMapping("/report")
	public ResponseEntity<List<DocumentByMonth>> report(
			@RequestParam GroupBy groupBy,
			@RequestParam(name = "startDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date startDate,
			@RequestParam(name = "endDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date endDate) {
		startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, -1);
		endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1);
		return new ResponseEntity<>(hsFolderDocService.report(startDate, endDate, groupBy), HttpStatus.OK);
	}

	@PostMapping("/delete")
	public ResponseEntity<?> deleteFolder(@RequestParam Long id, @RequestParam IconTypeEnum iconType) {
		switch (iconType) {
		case FOLDER:
		case PERSONAL:
		case ORG:
		case SHARE:
			hsFolderService.deleteFolderById(id);
			break;
		case DOC:
			hsFolderDocService.deleteDocumentById(id);
			break;
		case FILE:
			hsFolderFileService.deleteFileById(id);
			break;
		default:
			throw new RestExceptionHandler(Message.WRONG_INPUT_DATA);
		}
		return new ResponseEntity<>(true, HttpStatus.OK);
	}

	@GetMapping("/getFolderDetailById")
	public ResponseEntity<?> getFolderDetailById(@RequestParam Long folderId) {
		return new ResponseEntity<>(hsFolderService.getFolderDetailById(folderId), HttpStatus.OK);
	}

//	@GetMapping("/getDataBasicByFolderId")
//	public ResponseEntity<FolderDataBasicDto> getDataBasicByFolderId(@RequestParam(required = false) Long folderId) {
//		return new ResponseEntity<>(hsFolderService.getDataBasicByFolderId(folderId), HttpStatus.OK);
//	}

	@GetMapping("/getDataDetailByFolderId")
	public ResponseEntity<FolderDataDetailDto> getDataDetailByFolderId(@RequestParam(required = false) Long folderId,
			@RequestParam(defaultValue = "LUU_TRU_CA_NHAN", required = false) FolderTypeEnum type) {
		return new ResponseEntity<>(hsFolderService.getDataDetailByFolderId(folderId, type), HttpStatus.OK);
	}
	
	@GetMapping("/getDetailByFolderId")
	public ResponseEntity<FolderDetailDto> getDetailByFolderId(@RequestParam(required = false) Long folderId,
			@RequestParam(defaultValue = "LUU_TRU_CA_NHAN", required = false) FolderTypeEnum type) {
		return new ResponseEntity<>(hsFolderService.getDetailByFolderId(folderId, type), HttpStatus.OK);
	}

	// Get danh sach ho so cong viec.
	@GetMapping("/getListHSCV")
	public ResponseEntity<?> getListHSCV(@RequestParam Integer tab,
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam(required = false) Long orgQLId,
			@RequestParam(required = false) Long createBy,
			@RequestParam(required = false) String folderName,
			@RequestParam(required = false) Integer monthCreate,
			@RequestParam(required = false) Integer yearCreate,
			@RequestParam(required = false) String maHoso,
			@RequestParam(required = false) Long thoiHanId,
			@RequestParam(defaultValue = "LUU_TRU_CO_QUAN", required = false) FolderTypeEnum type) {
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(hsFolderService.getListHoso(orgQLId, createBy, folderName, monthCreate, yearCreate, maHoso, thoiHanId, tab, 1, type, pageable), HttpStatus.OK);
	}

	// Get danh sach ho so phong ban.
	@GetMapping("/getListHSPB")
	public ResponseEntity<?> getListHSPB(@RequestParam Integer tab,
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam(required = false) Long orgQLId,
			@RequestParam(required = false) Long createBy,
			@RequestParam(required = false) String folderName,
			@RequestParam(required = false) Integer monthCreate,
			@RequestParam(required = false) Integer yearCreate,
			@RequestParam(required = false) String maHoso,
			@RequestParam(required = false) Long thoiHanId,
			@RequestParam(defaultValue = "LUU_TRU_CO_QUAN", required = false) FolderTypeEnum type) {
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(hsFolderService.getListHoso(orgQLId, createBy, folderName, monthCreate, yearCreate, maHoso, thoiHanId, tab, 2, type, pageable), HttpStatus.OK);
	}

	// Get danh sach ho so co quan.
	@GetMapping("/getListHSCQ")
	public ResponseEntity<?> getListHSCQ(@RequestParam Integer tab,
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam(required = false) Long orgQLId,
			@RequestParam(required = false) Long createBy,
			@RequestParam(required = false) String folderName,
			@RequestParam(required = false) Integer monthCreate,
			@RequestParam(required = false) Integer yearCreate,
			@RequestParam(required = false) String maHoso,
			@RequestParam(required = false) Long thoiHanId,
			@RequestParam(defaultValue = "LUU_TRU_CO_QUAN", required = false) FolderTypeEnum type) {
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(hsFolderService.getListHoso(orgQLId, createBy, folderName, monthCreate, yearCreate, maHoso, thoiHanId, tab, 3, type, pageable), HttpStatus.OK);
	}

	// Get danh sach van ban va file trong ho so.
	@GetMapping("/getListVBByFolderId")
	public ResponseEntity<?> getListVBByFolderId(@RequestParam Long folderId,
												 @RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
												 @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
												 @RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
												 @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(hsFolderService.getListVBByFolderId(folderId, pageable), HttpStatus.OK);
	}

	@GetMapping("/getListRootFolder")
	public ResponseEntity<?> getListRootFolder(@RequestParam(required = false, defaultValue = "LUU_TRU_CA_NHAN") FolderTypeEnum type ) {
		return new ResponseEntity<>(hsFolderService.getListRootFolder(type), HttpStatus.OK);
	}

	@GetMapping("/transferToHSCV")
	public ResponseEntity<?> transferToHSCV(@RequestParam Long folderId, @RequestParam Long userApprove, @RequestParam(required = false) Long maintenance) {
		return new ResponseEntity<>(hsFolderService.transferToHSCV(folderId, userApprove, maintenance), HttpStatus.OK);
	}

	@GetMapping("/finishHSCV")
	public ResponseEntity<?> finishHSCV(@RequestParam Long folderId) {
		return new ResponseEntity<>(hsFolderService.finishHSCV(folderId), HttpStatus.OK);
	}

	@GetMapping("/transferToHSCQ")
	public ResponseEntity<?> transferToHSCQ(@RequestParam Long folderId, @RequestParam(required = false) Long orgApprove) {
		return new ResponseEntity<>(hsFolderService.transferToHSCQ(folderId, orgApprove), HttpStatus.OK);
	}

	//option = 1 => Phòng ban - option = 2 => Cơ quan
	@GetMapping("/review")
	public ResponseEntity<?> review(@RequestParam Long folderId, @RequestParam boolean approve,
			@RequestParam String comment, @RequestParam Long option, @RequestParam(required = false) String organld) {
		return new ResponseEntity<>(hsFolderService.review(folderId, approve, comment, option, organld), HttpStatus.OK);
	}
	
	@GetMapping("/getHosoByDocIdAndDocType")
	public ResponseEntity<?> getHosoByDocIdAndDocType(@RequestParam Long docId, @RequestParam DocumentTypeEnum docType) {
		return new ResponseEntity<>(hsFolderDocService.getHosoByDocIdAndDocType(docId, docType), HttpStatus.OK);
	}

	@GetMapping("/report/menu")
	public ResponseEntity<ReportMenuDto> getReportMenu() {
		return new ResponseEntity<>(hsFolderService.getReportMenu(), HttpStatus.OK);
	}

	@GetMapping("/search/criteria")
	public ResponseEntity<CriteriaSearchDto> getCriteriaSearch() {
		return new ResponseEntity<>(hsFolderService.getCriteriaSearch(), HttpStatus.OK);
	}
}
