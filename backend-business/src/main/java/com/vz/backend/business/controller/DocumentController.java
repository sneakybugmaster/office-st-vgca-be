package com.vz.backend.business.controller;

import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.websocket.server.PathParam;

import com.vz.backend.business.service.*;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.jpa.domain.JpaSort;
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

import com.vz.backend.business.domain.Attachment;
import com.vz.backend.business.domain.DocumentBook;
import com.vz.backend.business.domain.DocumentInProcess;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.business.dto.DocInRetakeDto;
import com.vz.backend.business.dto.DocumentDetail;
import com.vz.backend.business.dto.DocumentDto;
import com.vz.backend.business.dto.DocumentOutProcessDto;
import com.vz.backend.business.dto.FindDocDto;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.ActionEnum;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DeadlineHandleTypeEnum;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentInTrackingEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.dto.ListObjectDto;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.core.exception.RestForbidden;
import com.vz.backend.core.service.CategoryService;
import com.vz.backend.core.service.IService;
import com.vz.backend.core.service.RoleService;
import com.vz.backend.core.service.StraceSystemService;
import com.vz.backend.core.service.UserService;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;

@RestController
@RequestMapping("/document")
public class DocumentController {

	@Value("${configs.doc-in.return-previous-node: false}")
	private boolean returnPreviousNode;

	private static DocumentStatusEnum[] docStatusRetake = { DocumentStatusEnum.RETURN_DOC, DocumentStatusEnum.DOING,
			DocumentStatusEnum.DONE };
	private static DocumentStatusEnum[] docStatusRetakenn = { DocumentStatusEnum.RETURN_DOC, DocumentStatusEnum.DOING};
	private static DocumentStatusEnum[] docStatusRetaken = { DocumentStatusEnum.RETAKE_DOC };
	enum SortBy {
		UPDATEDATE("updateDate"), // Ngày cập nhật
		CREATEDATE("createDate"), // Ngày tạo
		NUMBER_ARRIVAL("numberArrival"), // số đến
		DATE_ARRIVAL("dateArrival"), // Ngày văn bản
		DATE_RECEIVED("receivedDate"), // Ngày nhận văn bản
		DEADLINE("deadline"), // hạn xử lý
		DOCID("id"), NUMBERSIGN("numberOrSign"), // số kí hiệu
		PREVIEW("preview"), // trích yếu
		ORG_ISSUED_NAME("placeSend"), // tổ chức ban hành
		ORG_HANDLE("p.orgName"), // đơn vị xử lý
		DOCTYPE("docTypeId"), DATEISSUED("dateIssued"), // ngày vào sổ
		STATUS("status"), // trạng thái văn bản (tab phối hợp)
		PERSON_HANDLE("p.toUsers.fullName"), // người xử lý ( tab phối hợp)
		DELEGATER("p.toUsers.fullName"), PROGRESS("p.progress"), // tiến độ
		SECURITY_NAME("security.name"), // độ mật
		IMPORTANT("(CASE WHEN important IS NULL THEN 3 WHEN important is FALSE THEN 2 ELSE 1 END)"), // Quan trọng
		ACTIVE("du.active"), PROCESS_STATUS("p.handleStatus"), // trạng thái xử lý
		// for supervisor
		ORG("placeSends.name");

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
			return SortBy.CREATEDATE.field;
		}
	}

	enum DocInRetakeSortEnum {
		UPDATE_DATE("updateDate"), NUMBER_ARRIVAL("documentBook.numberOrSign", "numberArrival"),
		DATE_ISSUED("dateIssued"), RETAKE_DATE("updateDate"), NUMBER_SIGN("numberOrSign"), PREVIEW("preview"),
		PLACE_SEND("placeSend");

		private String[] field;

		private DocInRetakeSortEnum(String... field) {
			this.field = field;
		}
	}

	@Autowired
	DocumentService docService;

	@Autowired
	private RoleService roleService;

	@Autowired
	private StraceSystemService straceService;

	@Autowired
	CategoryService categoryService;

	@Autowired
	RoleService rService;

	@Autowired
	UserService userService;

	@Autowired
	DocumentInTrackingService trackingServices;

	@Autowired
	DocumentInProcessService processService;

	@Autowired
	DocumentBookService dbService;

	@Autowired
	NotificationService noticeService;

	@Autowired
	ObjectReadService objReadService;

	@Autowired
	BpmnService2 bpmnService2;

	public IService<Documents> getService() {
		return docService;
	}

	@GetMapping(value = "/getDetailById/{id}")
	public ResponseEntity<?> getDetailById(@PathVariable Long id,
			@RequestParam(required = false) Long idNotification,
			@RequestParam(required = false) String tab) {
		tab = BussinessCommon.convert(tab);
		DocumentDetail documentDetail = docService.findDocumentDetailByClientIdAndId(id);
		if (documentDetail == null) {
			return new ResponseEntity<>(HttpStatus.NOT_FOUND);
		}
		User u = BussinessCommon.getUser();

//		if (!docService.checkPermission(id)) {
//			throw new RestExceptionHandler(Message.NO_PERMISSION);
//		}

		DocumentInProcess oProcess = processService.findByToUserAndDocId(u.getId(), id);
		if (oProcess != null) {
			// #2757 Văn thư đơn vị không được quyền xem văn bản mật
			if (Boolean.TRUE.equals(documentDetail.getDocument().getConfidential())
					&& Constant.START_STEP.equals(oProcess.getStep())) {
				throw new RestExceptionHandler(Message.NO_PERMISSION);
			}

			if(processService.isStatus(oProcess.getHandleStatus(), DocumentInHandleStatusEnum.CHO_XU_LY)) {
				processService.update(oProcess, DocumentInHandleStatusEnum.DANG_XU_LY);
			} else {
				processService.save(oProcess);
			}
		}

		// notification
		if (idNotification != null) {
			noticeService.setReadById(idNotification);
		} else {
			noticeService.setReadByDocIdAndDocTypeAndUserId(true, id, DocumentTypeEnum.VAN_BAN_DEN, u.getId());
		}

		trackingServices.save(id, tab != null ? DocumentInTrackingEnum.READ_UQ : DocumentInTrackingEnum.READ,
				u.getId());
		objReadService.setRead(BussinessCommon.getUserId(), id, DocumentTypeEnum.VAN_BAN_DEN, true);
		return new ResponseEntity<>(documentDetail, HttpStatus.OK);
	}

	@GetMapping("/getById/{id}")
	public ResponseEntity<?> getById(@PathVariable Long id) {
		Documents data = docService.validDocId(id);
		if (!docService.checkPermission(id)) {
			throw new RestExceptionHandler(Message.NO_PERMISSION);
		}
		objReadService.setRead(BussinessCommon.getUserId(), id, DocumentTypeEnum.VAN_BAN_DEN, true);

		// Attachment of parent doc - For case org transfer
		List<Attachment> atms = data.getAttachments();
		Documents parent = data.getParent();
		getAttachmentsByParent(parent,atms);

		data.hideDataByConfidentialDoc();

		return new ResponseEntity<>(data, HttpStatus.OK);
	}
	private List<Attachment> getAttachmentsByParent(Documents parent, List<Attachment> atms){
		if (parent != null && !BussinessCommon.isEmptyList(parent.getAttachments())) {
			atms.addAll(parent.getAttachments());
			if (parent.getParent() != null && !BussinessCommon.isEmptyList(parent.getParent().getAttachments())) {
				getAttachmentsByParent(parent.getParent(),atms);
			}
		}
		return atms;
	}
	@PostMapping(value = "/add")
	public ResponseEntity<Documents> create(@RequestParam(required = false) Boolean receive,
			@RequestBody Documents doc) {
		User u = BussinessCommon.getUser();
		Documents newDoc = docService.createDocument(receive, doc, u, BussinessCommon.getClientId(), null, true);

		return new ResponseEntity<>(newDoc, HttpStatus.OK);
	}



	@GetMapping("/getWaitToReceive")
	public ResponseEntity<?> getWaitToReceive(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam(required = false) Boolean important, @RequestParam(required = false) Boolean expired,
			@RequestParam(required = false) String text, @RequestParam(required = false) String numberOrSign,
			@RequestParam(required = false) String preview, @RequestParam(required = false) Long docTypeId,
			@RequestParam(required = false) Long docFieldsId, @RequestParam(required = false) Long methodReceiptId) {
		if (!userService.isVanThuVBDen(BussinessCommon.getUser())) {
			throw new RestExceptionHandler(Message.ROLE_LIBRARIAN);
		}
		text = BussinessCommon.convert(text);
		Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(docService.getWaitToReceive(text, important, expired, numberOrSign, preview,
				docTypeId, docFieldsId, methodReceiptId, pageable), HttpStatus.OK);
	}

	@PostMapping(value = "/update/{id}")
	public ResponseEntity<?> update(@PathVariable Long id, @RequestBody Documents doc) {
		return new ResponseEntity<>(docService.update(id, doc), HttpStatus.OK);
	}

	@PostMapping(value = "/returnDoc/{docId}")
	public ResponseEntity<?> returnDoc(@PathVariable Long docId, @PathParam(value = "comment") String comment,
			@PathParam(value = "file") MultipartFile[] files) {
		if (returnPreviousNode) {
			docService.returnDoc3(docId, comment, files, false);
		} else {
			docService.returnDoc(docId, comment, files, false);
		}

		return new ResponseEntity<>(HttpStatus.OK);
	}

	@PostMapping("/retakeByStep")
	public ResponseEntity<?> retakeByStep(@RequestParam(value = "docId") Long docId,
			@RequestParam(required = false) String comment,
			@RequestParam(required = false) MultipartFile[] files,
			@RequestParam(required = false, defaultValue = "FALSE") Boolean isDelegate) {
		return new ResponseEntity<>(docService.retakeByStep(docId, comment, files, isDelegate), HttpStatus.OK);
	}

	/**
	 * Đơn vị cha từ chối văn bản chuyển đơn vị của đơn vị con
	 * @param docId
	 * @param comment
	 * @param files
	 * @param isDelegate
	 * @return
	 */
	@PostMapping(value = "/reject/{docId}")
	public ResponseEntity<?> reject(@PathVariable Long docId, @RequestParam(required = false) String comment,
			@RequestParam(required = false) MultipartFile[] files) {
		return new ResponseEntity<>(docService.reject(docId, comment, files), HttpStatus.OK);
	}

	@PostMapping(value = "/orgTransfer")
	public ResponseEntity<?> orgTransfer(@RequestParam() List<Long> docIds,
			@RequestParam(required = false) String comment, @RequestParam(required = false) List<Long> listOrg,
			@RequestParam Long node, @RequestParam(required = false, defaultValue = "FALSE") boolean isDelegate,
			@RequestParam(required = false, defaultValue = "FALSE") boolean special,
			@RequestParam MultipartFile[] files) {
		return new ResponseEntity<>(docService.orgTransfer(docIds, comment, listOrg, node, files, isDelegate, special),
				HttpStatus.OK);
	}

	@GetMapping(value = "/findBasic")
	public ResponseEntity<?> searchBasic(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(required = false) Boolean important, @RequestParam(value = "text") String text,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		text = BussinessCommon.convert(text);
		Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(
				docService.findReceive(null, important, text, null, null, null, null, null, pageable), HttpStatus.OK);
	}

	@GetMapping(value = "/findAdvance")
	public ResponseEntity<?> findAdvance(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(required = false) Boolean important, @RequestParam(required = false) Boolean expired,
			@PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
			@PathParam(value = "docTypeId") Long docTypeId, @PathParam(value = "docFieldsId") Long docFieldsId,
			@PathParam(value = "statusReceiptId") Long statusReceiptId,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		numberOrSign = BussinessCommon.convert(numberOrSign);
		preview = BussinessCommon.convert(preview);
		Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(docService.findReceive(expired, important, null, numberOrSign, preview, docTypeId,
				docFieldsId, statusReceiptId, pageable), HttpStatus.OK);
	}

	@GetMapping(value = "/findBasicExeDoc")
	public ResponseEntity<?> findBasicExeDoc(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@PathParam(value = "text") String text,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		text = BussinessCommon.convert(text);
		Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(
				docService.findProcessingDoc(null, 13, null, null, null, text, null, null, null, null, null, pageable),
				HttpStatus.OK);
	}

	@GetMapping(value = "/findAdvanceExeDoc")
	public ResponseEntity<?> findAdvanceExeDoc(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
			@PathParam(value = "docTypeId") Long docTypeId, @PathParam(value = "docFieldsId") Long docFieldsId,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		numberOrSign = BussinessCommon.convert(numberOrSign);
		preview = BussinessCommon.convert(preview);
		Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(docService.findProcessingDoc(null, 13, null, null, null, null, numberOrSign, preview,
				docTypeId, docFieldsId, null, pageable), HttpStatus.OK);
	}

	@GetMapping(value = "/findAdvanceDoneDoc")
	public ResponseEntity<?> findAdvanceDoneDoc(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
			@PathParam(value = "docTypeId") Long docTypeId, @PathParam(value = "docFieldsId") Long docFieldsId,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		numberOrSign = BussinessCommon.convert(numberOrSign);
		preview = BussinessCommon.convert(preview);
		Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(docService.findProcessingDoc(null, 12, null, null, null, null, numberOrSign, preview,
				docTypeId, docFieldsId, null, pageable), HttpStatus.OK);
	}

	@GetMapping(value = "/findBasicDoneDoc")
	public ResponseEntity<?> findBasicDoneDoc(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@PathParam(value = "text") String text,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		text = BussinessCommon.convert(text);
		Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(
				docService.findProcessingDoc(null, 12, null, null, null, text, null, null, null, null, null, pageable),
				HttpStatus.OK);
	}

	@GetMapping(value = "/findBasicAllDoc")
	public ResponseEntity<?> findBasicAllDoc(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@PathParam(value = "text") String text,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		text = BussinessCommon.convert(text);
		Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(docService.findAll(null, null, text, null, null, null, null, null, null, null, null, null, null,
				null, null, null, null, null, null, null, null, null, null, null, pageable), HttpStatus.OK);
	}

	@GetMapping(value = "/findAllDoc")
	public ResponseEntity<?> findAllDoc(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(required = false) Boolean important, @RequestParam(required = false) Boolean expired,
			@PathParam(value = "userExe") String userExe, @PathParam(value = "numberOrSign") String numberOrSign,
			@PathParam(value = "preview") String preview, @PathParam(value = "bookId") Long bookId,
			@PathParam(value = "docTypeId") Long docTypeId, @PathParam(value = "orgIssuedName") String orgIssuedName,
			@PathParam(value = "docFieldsId") Long docFieldsId, @PathParam(value = "docStatusId") String docStatusId,
			@PathParam(value = "urgentId") Long urgentId, @PathParam(value = "securityId") Long securityId,
			@PathParam(value = "numberArrival") String numberArrival, @PathParam(value = "orgExe") String orgExe,
			@PathParam(value = "startArrival") @DateTimeFormat(iso = ISO.DATE) Date startArrival,
			@PathParam(value = "endArrival") @DateTimeFormat(iso = ISO.DATE) Date endArrival,
			@PathParam(value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
			@PathParam(value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
			@PathParam(value = "startReceived") @DateTimeFormat(iso = ISO.DATE) Date startReceived,
			@PathParam(value = "endReceived") @DateTimeFormat(iso = ISO.DATE) Date endReceived,
			@PathParam(value = "personSign") String personSign,
			@PathParam(value = "handleType") HandleTypeEnum handleType,
			@PathParam(value = "handleStatus") DocumentInHandleStatusEnum handleStatus,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		userExe = BussinessCommon.convert(userExe);
		numberOrSign = BussinessCommon.convert(numberOrSign);
		preview = BussinessCommon.convert(preview);
		orgIssuedName = BussinessCommon.convert(orgIssuedName);

		startArrival = DateTimeUtils.handleSubmit(startArrival, Calendar.MILLISECOND, -1);//Ngày văn bản
		endArrival = DateTimeUtils.handleSubmit(endArrival, Calendar.DAY_OF_MONTH, 1);//Ngày văn bản

		startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);//Ngày vào sổ
		endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);//Ngày vào sổ

		startReceived = DateTimeUtils.handleSubmit(startReceived, Calendar.MILLISECOND, -1);//Ngày nhận văn bản
		endReceived = DateTimeUtils.handleSubmit(endReceived, Calendar.DAY_OF_MONTH, 1);//Ngày nhận văn bản

		numberArrival = BussinessCommon.convert(numberArrival);
		personSign = BussinessCommon.convert(personSign);
		orgExe = BussinessCommon.convert(orgExe);
		userExe = BussinessCommon.convert(userExe);
		handleType = HandleTypeEnum.NULL.equals(handleType) ? null : handleType;
		handleStatus = DocumentInHandleStatusEnum.NULL.equals(handleStatus) ? null : handleStatus;
		Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(docService.findAll(expired, important, null, userExe, numberOrSign, preview, bookId,
				docTypeId, orgIssuedName, docFieldsId, DocumentStatusEnum.getEnum(docStatusId), urgentId, securityId,
				numberArrival, orgExe, startArrival, endArrival, startIssued, endIssued, startReceived, endReceived,
				personSign, handleType, handleStatus, pageable), HttpStatus.OK);
	}

	@GetMapping(value = "/exportExcel")
	public ResponseEntity<?> exportExcel(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(required = false) Boolean important, @RequestParam(required = false) Boolean expired,
			@PathParam(value = "text") String text, @PathParam(value = "userExe") String userExe,
			@PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
			@PathParam(value = "bookId") Long bookId, @PathParam(value = "docTypeId") Long docTypeId,
			@PathParam(value = "orgIssuedName") String orgIssuedName,
			@PathParam(value = "docFieldsId") Long docFieldsId, @PathParam(value = "docStatusId") String docStatusId,
			@PathParam(value = "urgentId") Long urgentId, @PathParam(value = "securityId") Long securityId,
			@PathParam(value = "numberArrival") String numberArrival, @PathParam(value = "orgExe") String orgExe,
			@PathParam(value = "startArrival") @DateTimeFormat(iso = ISO.DATE) Date startArrival,
			@PathParam(value = "endArrival") @DateTimeFormat(iso = ISO.DATE) Date endArrival,
			@PathParam(value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
			@PathParam(value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
			@PathParam(value = "startReceived") @DateTimeFormat(iso = ISO.DATE) Date startReceived,
			@PathParam(value = "endReceived") @DateTimeFormat(iso = ISO.DATE) Date endReceived,
			@PathParam(value = "personSign") String personSign,
			@PathParam(value = "handleType") HandleTypeEnum handleType,
			@PathParam(value = "handleStatus") DocumentInHandleStatusEnum handleStatus,
			@RequestParam(required = false) Boolean configFields) {
		userExe = BussinessCommon.convert(userExe);
		numberOrSign = BussinessCommon.convert(numberOrSign);
		preview = BussinessCommon.convert(preview);
		orgIssuedName = BussinessCommon.convert(orgIssuedName);

		startArrival = DateTimeUtils.handleSubmit(startArrival, Calendar.MILLISECOND, -1);//Ngày văn bản
		endArrival = DateTimeUtils.handleSubmit(endArrival, Calendar.DAY_OF_MONTH, 1);//Ngày văn bản

		startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);//Ngày vào sổ
		endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);//Ngày vào sổ

		startReceived = DateTimeUtils.handleSubmit(startReceived, Calendar.MILLISECOND, -1);//Ngày nhận văn bản
		endReceived = DateTimeUtils.handleSubmit(endReceived, Calendar.DAY_OF_MONTH, 1);//Ngày nhận văn bản

		numberArrival = BussinessCommon.convert(numberArrival);
		personSign = BussinessCommon.convert(personSign);
		orgExe = BussinessCommon.convert(orgExe);
		userExe = BussinessCommon.convert(userExe);
		handleType = HandleTypeEnum.NULL.equals(handleType) ? null : handleType;
		handleStatus = DocumentInHandleStatusEnum.NULL.equals(handleStatus) ? null : handleStatus;
		Sort sort = Sort.by(direction, SortBy.getEnum(sortBy));
		if (Boolean.TRUE.equals(configFields)) {
			return new ResponseEntity<>(docService.exportExcelByConfig(expired,important, text, userExe, numberOrSign, preview, bookId,
					docTypeId, orgIssuedName, docFieldsId, DocumentStatusEnum.getEnum(docStatusId), urgentId,
					securityId, numberArrival, orgExe, startArrival, endArrival, startIssued, endIssued, startReceived,
					endReceived, personSign, handleType, handleStatus, sort), HttpStatus.OK);
		}
		return new ResponseEntity<>(docService.exportExcel(expired,important, text, userExe, numberOrSign, preview, bookId, docTypeId,
				orgIssuedName, docFieldsId, DocumentStatusEnum.getEnum(docStatusId), urgentId, securityId,
				numberArrival, orgExe, startArrival, endArrival, startIssued, endIssued, startReceived, endReceived,
				personSign, handleType, handleStatus, sort), HttpStatus.OK);
	}

	@GetMapping(value = "/getNumberArrival")
	public ResponseEntity<?> getNumberArrival() {
		return new ResponseEntity<>(dbService.getMaxNumberVBDen(), HttpStatus.OK);
	}

	@GetMapping(value = "/findDocReply")
	public ResponseEntity<?> findDocReply(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
			@PathParam(value = "orgIssuedName") String orgIssuedName,
			@PathParam(value = "docStatusId") String docStatusId,
			@PathParam(value = "startArrival") @DateTimeFormat(iso = ISO.DATE) Date startArrival,
			@PathParam(value = "endArrival") @DateTimeFormat(iso = ISO.DATE) Date endArrival,
			@PathParam(value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
			@PathParam(value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		numberOrSign = BussinessCommon.convert(numberOrSign);
		preview = BussinessCommon.convert(preview);
		orgIssuedName = BussinessCommon.convert(orgIssuedName);
		startArrival = DateTimeUtils.handleSubmit(startArrival, Calendar.MILLISECOND, -1);
		endArrival = DateTimeUtils.handleSubmit(endArrival, Calendar.DAY_OF_MONTH, 1);
		startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);
		endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);
		Pageable pageable = BussinessCommon.castToPageable(page, Sort.by(direction, SortBy.getEnum(sortBy)), size);
		return new ResponseEntity<>(docService.findDocReply(numberOrSign, preview, orgIssuedName,
				DocumentStatusEnum.getEnum(docStatusId), startArrival, endArrival, startIssued, endIssued, pageable),
				HttpStatus.OK);
	}

	@GetMapping(value = "/find_process_doc")
	public ResponseEntity<?> findProcessingDoc(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@PathParam(value = "text") String text, @PathParam(value = "numberOrSign") String numberOrSign,
			@PathParam(value = "preview") String preview, @PathParam(value = "docTypeId") Long docTypeId,
			@PathParam(value = "docFieldsId") Long docFieldsId,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		text = BussinessCommon.convert(text);
		numberOrSign = BussinessCommon.convert(numberOrSign);
		preview = BussinessCommon.convert(preview);
		Pageable pageable = BussinessCommon.castToPageable(page, Sort.by(direction, SortBy.getEnum(sortBy)), size);
		return new ResponseEntity<>(docService.findProcessingDoc(null, 11, null, null, null, text, numberOrSign, preview,
				docTypeId, docFieldsId, null, pageable), HttpStatus.OK);
	}

	@PostMapping("/delete/{id}")
	public ResponseEntity<?> delete(@PathVariable Long id) {
		User u = BussinessCommon.getUser();
		boolean isSuperAdmin = roleService.existUserInModule(ModuleCodeEnum.DOC_IN_RETAKE.getName());
		boolean isLibrarian = userService.isVanThuVBDen(u);

		if (!isLibrarian && !isSuperAdmin) {
			throw new RestExceptionHandler(Message.NO_PROCESS_HANDLE);
		}

		Documents doc = docService.validDocId(id);
		if (isLibrarian && !isSuperAdmin && !DocumentStatusEnum.NOT_YET.equals(doc.getStatus()) && doc.getParentId() != null) {
			throw new RestExceptionHandler(Message.NO_DELETE_DOC);
		}

		try {
			processService.updateByActive(id, false);
			docService.deleteDoc(doc);
			return ResponseEntity.status(HttpStatus.OK).build();
		} catch (Exception e) {
			return ResponseEntity.badRequest().build();
		}
	}

	@PostMapping(value = "/retake/{docId}")
	public ResponseEntity<?> retake(@PathVariable Long docId, @PathParam(value = "comment") String comment,
			@PathParam(value = "file") MultipartFile[] files) {
		docService.retake(docId, comment, files, false);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@PostMapping(value = "/updateDeadline/{docId}")
	public ResponseEntity<?> updateDeadline(@PathVariable Long docId,
			@RequestParam(required = true) @DateTimeFormat(iso = ISO.DATE) Date deadline,
			@RequestParam(required = true) DeadlineHandleTypeEnum type) {
		deadline = DateTimeUtils.handleSubmit(deadline);
		docService.updateDeadline(docId, deadline, type);
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@GetMapping(value = "/findDocByTypeHandle/{type}/{status}")
	public ResponseEntity<ListObjectDto<DocumentDto>> findDocByTypeHandle(
			@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(required = false) Boolean important, @RequestParam(required = false) Boolean expired,
			@PathVariable Integer type, @PathVariable Integer status, @PathParam(value = "text") String text,
			@PathParam(value = "preview") String preview, @PathParam(value = "docTypeId") Long docTypeId,
			@PathParam(value = "docFieldsId") Long docFieldsId,
			@PathParam(value = "posId") Long posId,
			@RequestParam(value = "dayLeft", required = false) Integer dayLeft,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		text = BussinessCommon.convert(text);
		preview = BussinessCommon.convert(preview);
		Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
		Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
		return new ResponseEntity<>(docService.findProcessingDoc(dayLeft, type, expired, important, status, text, null, preview,
				docTypeId, docFieldsId, posId, pageable), HttpStatus.OK);
	}

	@GetMapping("/list-retake")
	public ResponseEntity<Page<DocInRetakeDto>> listRetake(
			@RequestParam(defaultValue = "UPDATE_DATE") DocInRetakeSortEnum sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		requirePermission();

		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(docService.listRetake(docStatusRetakenn, pageable), HttpStatus.OK);
	}

	@GetMapping("/list-retaken")
	public ResponseEntity<Page<DocInRetakeDto>> listRetaken(
			@RequestParam(defaultValue = "UPDATE_DATE") DocInRetakeSortEnum sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		requirePermission();

		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(docService.listRetake(docStatusRetaken, pageable), HttpStatus.OK);
	}

	@GetMapping("/quick-retake")
	public ResponseEntity<Page<DocInRetakeDto>> quickRetake(
			@RequestParam(defaultValue = "UPDATE_DATE") DocInRetakeSortEnum sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size, @RequestParam(required = false) String q,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		q = StringUtils.handleSubmit(q);
		requirePermission();

		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(docService.quickRetake(docStatusRetakenn, q, pageable), HttpStatus.OK);
	}

	@GetMapping("/quick-retaken")
	public ResponseEntity<Page<DocInRetakeDto>> quickRetaken(
			@RequestParam(defaultValue = "UPDATE_DATE") DocInRetakeSortEnum sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size, @RequestParam(required = false) String q,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		q = StringUtils.handleSubmit(q);
		requirePermission();

		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(docService.quickRetake(docStatusRetaken, q, pageable), HttpStatus.OK);
	}

	@GetMapping("/search-retake")
	public ResponseEntity<Page<DocInRetakeDto>> searchRetake(
			@RequestParam(defaultValue = "UPDATE_DATE") DocInRetakeSortEnum sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam(required = false, value = "numberOrSign") String numberOrSign,
			@RequestParam(required = false, value = "preview") String preview,
			@RequestParam(required = false, value = "bookId") Long bookId,
			@RequestParam(required = false, value = "docTypeId") Long docTypeId,
			@RequestParam(required = false, value = "orgIssuedName") String orgIssuedName,
			@RequestParam(required = false, value = "docFieldsId") Long docFieldsId,
			@RequestParam(required = false, value = "urgentId") Long urgentId,
			@RequestParam(required = false, value = "securityId") Long securityId,
			@RequestParam(required = false, value = "numberArrival") String numberArrival,
			@RequestParam(required = false, value = "startArrival") @DateTimeFormat(iso = ISO.DATE) Date startArrival,
			@RequestParam(required = false, value = "endArrival") @DateTimeFormat(iso = ISO.DATE) Date endArrival,
			@RequestParam(required = false, value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
			@RequestParam(required = false, value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
			@RequestParam(required = false, value = "personSign") String personSign) {
		numberOrSign = StringUtils.handleSubmit(numberOrSign);
		preview = StringUtils.handleSubmit(preview);
		orgIssuedName = StringUtils.handleSubmit(orgIssuedName);
		personSign = StringUtils.handleSubmit(personSign);
		numberArrival = StringUtils.handleSubmit(numberArrival);
		startArrival = DateTimeUtils.handleSubmit(startArrival, Calendar.MILLISECOND, -1);
		endArrival = DateTimeUtils.handleSubmit(endArrival, Calendar.DAY_OF_MONTH, 1);
		startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);
		endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);

		requirePermission();

		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(docService.searchRetake(docStatusRetake, pageable, numberOrSign, preview, bookId,
				docTypeId, orgIssuedName, docFieldsId, urgentId, securityId, numberArrival, startArrival, endArrival,
				startIssued, endIssued, personSign), HttpStatus.OK);
	}

	@GetMapping("/search-retaken")
	public ResponseEntity<Page<DocInRetakeDto>> searchRetaken(
			@RequestParam(defaultValue = "UPDATE_DATE") DocInRetakeSortEnum sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
			@RequestParam(required = false, value = "numberOrSign") String numberOrSign,
			@RequestParam(required = false, value = "preview") String preview,
			@RequestParam(required = false, value = "bookId") Long bookId,
			@RequestParam(required = false, value = "docTypeId") Long docTypeId,
			@RequestParam(required = false, value = "orgIssuedName") String orgIssuedName,
			@RequestParam(required = false, value = "docFieldsId") Long docFieldsId,
			@RequestParam(required = false, value = "urgentId") Long urgentId,
			@RequestParam(required = false, value = "securityId") Long securityId,
			@RequestParam(required = false, value = "numberArrival") String numberArrival,
			@RequestParam(required = false, value = "startArrival") @DateTimeFormat(iso = ISO.DATE) Date startArrival,
			@RequestParam(required = false, value = "endArrival") @DateTimeFormat(iso = ISO.DATE) Date endArrival,
			@RequestParam(required = false, value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
			@RequestParam(required = false, value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
			@RequestParam(required = false, value = "personSign") String personSign) {
		numberOrSign = StringUtils.handleSubmit(numberOrSign);
		preview = StringUtils.handleSubmit(preview);
		orgIssuedName = StringUtils.handleSubmit(orgIssuedName);
		personSign = StringUtils.handleSubmit(personSign);
		numberArrival = StringUtils.handleSubmit(numberArrival);
		startArrival = DateTimeUtils.handleSubmit(startArrival, Calendar.MILLISECOND, -1);
		endArrival = DateTimeUtils.handleSubmit(endArrival, Calendar.DAY_OF_MONTH, 1);
		startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);
		endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);

		requirePermission();

		Sort sort = Sort.by(direction, sortBy.field);
		Pageable pageable = PageRequest.of(page - 1, size, sort);
		return new ResponseEntity<>(docService.searchRetake(docStatusRetaken, pageable, numberOrSign, preview, bookId,
				docTypeId, orgIssuedName, docFieldsId, urgentId, securityId, numberArrival, startArrival, endArrival,
				startIssued, endIssued, personSign), HttpStatus.OK);
	}

	@PostMapping(value = "/force-retake/{docId}")
	public ResponseEntity<Documents> forceRetake(@PathVariable Long docId, @PathParam(value = "comment") String comment,
			@PathParam(value = "file") MultipartFile[] files) {
//		requirePermission();
		docService.retake(docId, comment, files, true);
		trackingServices.save(docId, DocumentInTrackingEnum.RETAKE, BussinessCommon.getUserId());
		return new ResponseEntity<>(HttpStatus.OK);
	}

	private void requirePermission() {
		if (roleService.existUserInModule(ModuleCodeEnum.DOC_IN_RETAKE.getName())) {
			return;
		}
		throw new RestForbidden("No permission for this action");
	}

	@PostMapping("/find_all/{page}")
	public ResponseEntity<?> findAll(@PathVariable Integer page, @RequestBody FindDocDto dto) {
		return new ResponseEntity<>(docService.findAll(dto, page), HttpStatus.OK);
	}

	@PostMapping(value = "/transferHandleList")
	public ResponseEntity<?> transferHandleList(@PathParam(value = "docIds") Long[] docIds,
			@RequestParam(required = false, defaultValue = "FALSE") Boolean requestReview,
			@PathParam(value = "comment") String comment, @PathParam(value = "cmtContent") String cmtContent,
			@RequestParam(required = false) String[] main, @RequestParam(required = false) Long[] direction,
			@RequestParam(required = false) String[] support, @RequestParam(required = false) String[] show,
			@RequestParam(required = false) Long[] org_main, @RequestParam(required = false) Long[] org_support,
			@RequestParam(required = false) Long[] org_show,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date deadline,
			@PathParam(value = "node") Long node, @PathParam(value = "files") MultipartFile[] files) {
		deadline = deadline == null ? null : DateTimeUtils.handleSubmit(deadline);
		if (!BussinessCommon.isEmptyArr(docIds) && docIds.length == 1) {
			return new ResponseEntity<>(docService.transfer2(docIds[0], comment, cmtContent, main, support, show, node,
					files, org_main, org_support, org_show, deadline, direction, requestReview), HttpStatus.OK);
		}
		return new ResponseEntity<>(docService.transferHandleList(docIds, comment, cmtContent, main, support, show,
				node, files, org_main, org_support, org_show, deadline, direction, requestReview), HttpStatus.OK);
	}

	@PostMapping(value = "/restore/{docId}")
	public ResponseEntity<Documents> restore(@PathVariable Long docId, @PathParam(value = "comment") String comment,
			@PathParam(value = "files") MultipartFile[] files) {
		requirePermission();
		docService.restore(docId, comment, files);
		trackingServices.save(docId, DocumentInTrackingEnum.RESTORE, BussinessCommon.getUserId());
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@GetMapping(value = "/getListDelegatedDocs/{type}/{status}")
	public ResponseEntity<?> getListDelegatedDocs(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
			@RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
			@RequestParam(required = false) Boolean important, @PathVariable Integer type, @PathVariable Integer status,
			@PathParam(value = "text") String text, @PathParam(value = "preview") String preview,
			@PathParam(value = "docTypeId") Long docTypeId, @PathParam(value = "docFieldsId") Long docFieldsId,
			@RequestParam(value = "dayLeft", required = false) Integer dayLeft,
			@RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
			@RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
		text = BussinessCommon.convert(text);
		preview = BussinessCommon.convert(preview);
		Pageable pageable = BussinessCommon.castToPageable(page, Sort.by(direction, SortBy.getEnum(sortBy)), size);
		return new ResponseEntity<>(
				docService.getListDelegatedDocs(dayLeft, important, type, status, text, null, preview, docTypeId, docFieldsId, pageable), HttpStatus.OK);
	}

	@PostMapping(value = "/delegate_transfer/{docId}")
	public ResponseEntity<?> delegate_transfer(@PathVariable Long docId, @PathParam(value = "comment") String comment,
			@PathParam(value = "cmtContent") String cmtContent,
			@RequestParam(required = false) String[] main, @RequestParam(required = false) String[] support,
			@RequestParam(required = false) String[] show, @RequestParam(required = false) Long[] org_main,
			@RequestParam(required = false) Long[] org_support, @RequestParam(required = false) Long[] org_show,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date deadline,
			@PathParam(value = "node") Long node, @PathParam(value = "files") MultipartFile[] files) {
		deadline = deadline == null ? null : DateTimeUtils.handleSubmit(deadline);
		return new ResponseEntity<>(docService.transfer2(docId, comment, cmtContent, main, support, show, node, files,
				org_main, org_support, org_show, deadline, null, false), HttpStatus.OK);
	}

	@PostMapping(value = "/delegate_reject/{docId}")
	public ResponseEntity<?> delegate_reject(@PathVariable Long docId, @PathParam(value = "comment") String comment,
			@PathParam(value = "file") MultipartFile[] files) {
		if (returnPreviousNode) {
			docService.returnDoc3(docId, comment, files, true);
		} else {
			docService.returnDoc(docId, comment, files, true);
		}
		return new ResponseEntity<>(HttpStatus.OK);
	}

	@PostMapping("/find_all_doc/{page}")
	public ResponseEntity<?> find_all_doc(@PathVariable int page, @RequestBody FindDocDto dto) {
		Pageable pageable = BussinessCommon.castToPageable(page,
				Sort.by(dto.getDirection(), SortBy.getEnum(dto.getSortBy())), dto.getPageSize());
		return new ResponseEntity<>(docService.findAllDoc(dto, pageable), HttpStatus.OK);
	}

	@GetMapping(value = "/ExcelFlowingIn")
	public ResponseEntity<?> ExcelFlowingIn(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
										 @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction, @PathParam(value = "expired") Boolean expired,
										 @PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
										 @PathParam(value = "bookId") Long bookId,
										 @PathParam(value = "orgIssuedName") String orgIssuedName,
										 @PathParam(value = "docFieldsId") Long docFieldsId, @PathParam(value = "docStatusId") String docStatusId,
										 @PathParam(value = "urgentId") Long urgentId, @PathParam(value = "securityId") Long securityId,
										 @PathParam(value = "numberArrival") Long numberArrival,
										 @PathParam(value = "startArrival") @DateTimeFormat(iso = ISO.DATE) Date startArrival,
										 @PathParam(value = "endArrival") @DateTimeFormat(iso = ISO.DATE) Date endArrival,
										 @PathParam(value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
										 @PathParam(value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
										 @PathParam(value = "startReceived") @DateTimeFormat(iso = ISO.DATE) Date startReceived,
										 @PathParam(value = "endReceived") @DateTimeFormat(iso = ISO.DATE) Date endReceived,
//										 @PathParam(value = "handleType") HandleTypeEnum handleType,
//										 @PathParam(value = "handleStatus") DocumentInHandleStatusEnum handleStatus,
										 @RequestParam(required = false) Long orgReceiveId,
										 @RequestParam(required = false) Boolean configFields) {
		numberOrSign = BussinessCommon.convert(numberOrSign);
		preview = BussinessCommon.convert(preview);
		orgIssuedName = BussinessCommon.convert(orgIssuedName);
		startArrival = DateTimeUtils.handleSubmit(startArrival, Calendar.MILLISECOND, -1);//Ngày văn bản
		endArrival = DateTimeUtils.handleSubmit(endArrival, Calendar.DAY_OF_MONTH, 1);//Ngày văn bản
		startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);//Ngày vào sổ
		endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);//Ngày vào s
		startReceived = DateTimeUtils.handleSubmit(startReceived, Calendar.MILLISECOND, -1);//Ngày nhận văn bản
		endReceived = DateTimeUtils.handleSubmit(endReceived, Calendar.DAY_OF_MONTH, 1);//Ngày nhận văn bản
//		handleType = HandleTypeEnum.NULL.equals(handleType) ? null : handleType;
//		handleStatus = DocumentInHandleStatusEnum.NULL.equals(handleStatus) ? null : handleStatus;
		Sort sort = Sort.by(direction, SortBy.getEnum(sortBy));
		return new ResponseEntity<>(docService.exportExcels(expired,numberOrSign, preview, bookId,
				orgIssuedName, docFieldsId, DocumentStatusEnum.getEnum(docStatusId), urgentId, securityId,
				numberArrival,startArrival, endArrival, startIssued, endIssued, startReceived, endReceived, orgReceiveId), HttpStatus.OK);
	}


	@PostMapping(value = "/org/retake/{docId}")
	public ResponseEntity<?> retakeOrgTransfer(@PathVariable Long docId,
			@RequestParam(required = false) String comment,
			@RequestParam(required = false) MultipartFile[] files) {
		return new ResponseEntity<>(docService.retakeOrgTransfer(docId, comment, files), HttpStatus.OK);
	}

	@PostMapping(value = "/requestReview/{docId}")
	public ResponseEntity<?> requestReview(@PathVariable Long docId,
			@RequestParam(required = false) String comment,
			@RequestParam(required = false) MultipartFile[] files,
			@RequestParam(required = false, defaultValue = "false") Boolean isDelegate) {
		return new ResponseEntity<>(docService.requestReview(docId, comment, files, isDelegate), HttpStatus.OK);
	}

	@PostMapping(value = "/review/{docId}")
	public ResponseEntity<?> review(@PathVariable Long docId,
			@RequestParam boolean agree,
			@RequestParam(required = false) String comment,
			@RequestParam(required = false) MultipartFile[] files,
			@RequestParam Long pId) {
		return new ResponseEntity<>(docService.review(docId, pId, agree, comment, files), HttpStatus.OK);
	}

	@PostMapping("/request_comment/{docId}")
	public ResponseEntity<?> requestComment(@PathVariable Long docId,
			@RequestParam(value = "orgId", required = false) Long[] orgId,
			@RequestParam(value = "toUserId", required = false) Long[] toUserId,
			@RequestParam(value = "comment", required = false) String comment,
			@RequestParam(required = false) MultipartFile[] files) {
		return new ResponseEntity<>(docService.requestComment(docId, orgId, toUserId, comment, files), HttpStatus.OK);
	}

	@PostMapping("/reply_comment/{docId}")
	public ResponseEntity<?> replyComment(@PathVariable Long docId,
			@RequestParam(value = "comment", required = false) String comment,
			@RequestParam(required = false) MultipartFile[] files) {
		return new ResponseEntity<>(docService.replyComment(docId, comment, files), HttpStatus.OK);
	}

	@GetMapping("/report_doc_by_type")
	public ResponseEntity<ReportDocByTypeDto> reportDocByType() {
		return new ResponseEntity<>(processService.reportDocByType(), HttpStatus.OK);
	}

	@GetMapping("/report_doc_delegate")
	public ResponseEntity<ReportDocByTypeDto> reportDocDelegate() {
		return new ResponseEntity<>(processService.reportDocDelegate(), HttpStatus.OK);
	}

	@PostMapping(value = "/switchOrAddUser/{docId}")
	public ResponseEntity<?> switchOrAddUser(@PathVariable Long docId,
			@RequestParam(required = false, value = "comment") String comment,
			@RequestParam(required = false, value = "cmtContent") String cmtContent,
			@RequestParam(required = false) String[] main, @RequestParam(required = false) String[] support,
			@RequestParam(required = false) String[] show, @RequestParam(required = false) Long[] org_main,
			@RequestParam(required = false) Long[] org_support, @RequestParam(required = false) Long[] org_show,
			@RequestParam(required = false) @DateTimeFormat(iso = ISO.DATE) Date deadline,
			@RequestParam(required = false) Long[] direction,
			@PathParam(value = "node") Long node, @PathParam(value = "files") MultipartFile[] files,
			@RequestParam(required = false, defaultValue = "false") boolean isSwitch) {
		return new ResponseEntity<>(docService.switchOrAddUser(docId, comment, cmtContent, main, support, show, node, files, org_main,
				org_support, org_show, deadline, isSwitch, direction), HttpStatus.OK);
	}

	@PostMapping("/retake_done/{docId}")
	public ResponseEntity<?> retakeDone(@PathVariable Long docId,
			@RequestParam(value = "comment", required = false) String comment,
			@RequestParam(required = false) MultipartFile[] files) {
		return new ResponseEntity<>(docService.retakeDone(docId, comment, files), HttpStatus.OK);
	}

	@GetMapping("/resolve-file/add/{docId}")
	public ResponseEntity<?> saveResolveFile(@PathVariable Long docId) {
		Documents doc = docService.validDocId(docId);
		return new ResponseEntity<>(docService.saveResolveFile(doc), HttpStatus.OK);
	}

	@PostMapping("/resolve-file/update/{docId}")
	public ResponseEntity<?> updateResolveFile(@PathVariable Long docId) {
		Documents doc = docService.validDocId(docId);
		return new ResponseEntity<>(docService.updateResolveFile(doc), HttpStatus.OK);
	}

	@GetMapping("/resolve-file/{docId}")
	public ResponseEntity<?> getUserInfoByDocId(@PathVariable Long docId) {
		return new ResponseEntity<>(docService.getUserInfoByDocId(docId), HttpStatus.OK);
	}

	/**
	 * show list node to choose node reject
	 * @param docId
	 * @return
	 */
	@GetMapping("/node-reject/list/{docId}")
	public ResponseEntity<?> nodeReject(@PathVariable Long docId) {
		return new ResponseEntity<>(processService.transferToUser(docId), HttpStatus.OK);
	}

	@PostMapping("/node-reject/do")
	public ResponseEntity<?> rejectByNodeId(@RequestParam Long pId, @RequestParam Long docId,
			@RequestParam String comment, @RequestParam(required = false) MultipartFile[] files) {
		return new ResponseEntity<>(docService.rejectByNodeId(pId, docId, comment, files), HttpStatus.OK);
	}

	@PostMapping("/node-retake/do")
	public ResponseEntity<?> retakeByNodeId(@RequestParam Long docId,
			@RequestParam(required = false) String comment, @RequestParam(required = false) MultipartFile[] files) {
		return new ResponseEntity<>(docService.retakeByNodeId(docId, comment, files), HttpStatus.OK);
	}

	@GetMapping("/node-review/list/{docId}")
	public ResponseEntity<List<DocumentOutProcessDto>> nodeRetake(@PathVariable Long docId) {
		return new ResponseEntity<>(processService.userTransferTo(docId), HttpStatus.OK);
	}

	@PostMapping(value = "/org/transfer")
	public ResponseEntity<?> orgTransfer(@RequestParam() List<Long> docIds,
			@RequestParam(required = false) String comment, @RequestParam(required = false) List<List<Long>> listOrg,
			@RequestParam Long node, @RequestParam MultipartFile[] files) {
		return new ResponseEntity<>(docService.orgTransfer2(docIds, comment, listOrg, node, files), HttpStatus.OK);
	}

	@PostMapping(value = "/org/reject/{docId}")
	public ResponseEntity<?> rejectReceiveDoc(@PathVariable Long docId, @RequestParam(required = false) String comment,
			@RequestParam(required = false) MultipartFile[] files) {
		return new ResponseEntity<>(docService.rejectReceiveDoc(docId, comment, files), HttpStatus.OK);
	}

} 
