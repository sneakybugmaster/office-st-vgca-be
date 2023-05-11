package com.vz.backend.business.controller;

import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import javax.websocket.server.PathParam;

import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import org.springframework.beans.factory.annotation.Autowired;
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

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.dto.FindDocDto;
import com.vz.backend.business.dto.KnowableDto;
import com.vz.backend.business.dto.ReportDocByTypeDto;
import com.vz.backend.business.dto.ShowToKnowDto;
import com.vz.backend.business.dto.document.IssuedDto;
import com.vz.backend.business.dto.fullreport.ProcessByMonth;
import com.vz.backend.business.service.DocumentOutProcessService;
import com.vz.backend.business.service.DocumentOutService;
import com.vz.backend.business.service.DocumentOutTrackingService;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.dto.ReportDto;
import com.vz.backend.core.dto.SignerDto;
import com.vz.backend.core.service.IService;
import com.vz.backend.util.DateTimeUtils;
import com.vz.backend.util.StringUtils;

import lombok.Getter;

@RestController
@RequestMapping("/document_out")
public class DocumentOutController {
    enum SortBy {
        UPDATEDATE("updateDate"), // ngày cập nhật
        CREATEDATE("createDate"), // ngày tạo
        DOCID("id"), NUMBERSIGN("numberOrSign"), // số-kí hiệu
        PREVIEW("preview"), // trích yếu
        DATEISSUED("dateIssued"), // ngày ban hành
        USER_ENTER("userEnter.fullName"), // người tạo
        DOCTYPE("docType.name"), // loại văn bản
        STATUS("status"), // trạng thái văn bản
        PERSON_HANDLE("p.user.fullName"), // người xử lý (tab vb trình kí -> tab đã trình kí)
        SECURITY_NAME("security.name"), // độ mật
        IMPORTANT("(CASE WHEN important IS NULL THEN 3 WHEN important is FALSE THEN 2 ELSE 1 END)"), // quan trọng
        ACTIVE("du.active"),
        // for supervisor
        ORG("org.name");

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

    @Getter
    public enum Tab {
        DA_XU_LY("Đã xử lý"),
        CHO_XU_LY("Chờ xử lý");

        private String field;

        private Tab(String field) {
            this.field = field;
        }
    }

    public enum GroupBy {
        MONTH(1), QUARTER(3), YEAR(12);

        int month;

        private GroupBy(int month) {
            this.month = month;
        }

        public int getMonth() {
            return month;
        }
    }

    @Autowired
    DocumentOutService docOutService;

    @Autowired
    DocumentOutProcessService docOutProcess;

    @Autowired
    DocumentOutTrackingService docOutTracking;

    public IService<DocumentOut> getService() {
        return docOutService;
    }

    @PostMapping("/add")
    public ResponseEntity<?> create(@RequestBody DocumentOut input) {
        input.setId(null);
        return new ResponseEntity<>(docOutService.create(input), HttpStatus.OK);
    }

    @PostMapping("/update/{id}")
    public ResponseEntity<?> update(@PathVariable Long id, @RequestBody DocumentOut input) {
        return new ResponseEntity<>(docOutService.update(id, input), HttpStatus.OK);
    }

    @PostMapping("/updateListSigners/{docId}")
    public ResponseEntity<?> updateListSigners(@PathVariable Long docId, @RequestParam String listSigners) {
        return new ResponseEntity<>(docOutService.updateListSigners(docId, listSigners), HttpStatus.OK);
    }

    @PostMapping("/delete/{id}")
    public ResponseEntity<?> delete(@PathVariable Long id) {
        return new ResponseEntity<>(docOutService.delete(id), HttpStatus.OK);
    }

    @GetMapping("/getDetailById/{docId}")
    public ResponseEntity<?> getDetailById(@PathVariable Long docId) {
        return new ResponseEntity<>(docOutService.getDetailById(docId), HttpStatus.OK);
    }

    @GetMapping("/getDetailToEdit/{docId}")
    public ResponseEntity<?> getDetailToEdit(@PathVariable Long docId) {
        return new ResponseEntity<>(docOutService.getDetailToEdit(docId), HttpStatus.OK);
    }

    @GetMapping("/getDetailToShow/{docId}")
    public ResponseEntity<?> getDetailToShow(@PathVariable Long docId) {
        return new ResponseEntity<>(docOutService.getDetailToShow(docId), HttpStatus.OK);
    }

    @GetMapping(value = "/getDataInit")
    public ResponseEntity<?> getDataInit(@RequestParam(value = "isInternalDocument", defaultValue = "false") Boolean isInternalDocument ) {
        return new ResponseEntity<>(docOutService.getDataInit(isInternalDocument), HttpStatus.OK);
    }

    @GetMapping(value = "/document_in_search_form_data")
    public ResponseEntity<?> getSearchDocForm() {
        return new ResponseEntity<>(docOutService.getSearchDocForm(), HttpStatus.OK);
    }

    @PostMapping("/request_sign/{docId}")
    public ResponseEntity<?> requestSign(@PathVariable Long docId, @PathParam(value = "toUserId") Long toUserId,
                                         @RequestParam(value = "delegateId", required = false) Long delegateId,
                                         @PathParam(value = "nodeId") Long nodeId, @RequestBody(required = false) String comment) {
        return new ResponseEntity<>(docOutService.requestSign(docId, BussinessCommon.getUser().getId(), toUserId,
                delegateId, nodeId, comment), HttpStatus.OK);
    }

    @PostMapping("/request_multiple_sign/{docId}")
    public ResponseEntity<?> requestMultipleSign(@RequestBody List<Long> docIds, @PathParam(value = "toUserId") Long toUserId,
                                                 @RequestParam(value = "delegateId", required = false) Long delegateId,
                                                 @PathParam(value = "nodeId") Long nodeId, @RequestBody(required = false) String comment) {
        return new ResponseEntity<>(docOutService.requestMultipleSign(docIds, BussinessCommon.getUser().getId(), toUserId,
                delegateId, nodeId, comment), HttpStatus.OK);
    }

    @PostMapping("/request_comment")
    public ResponseEntity<?> requestComment(@RequestParam(value = "docId") String docId,
                                            @RequestParam(value = "listToUserId") String listToUserId,
                                            @RequestParam(value = "comment", required = false) String comment) {
        return new ResponseEntity<>(docOutService.requestComment(Long.parseLong(docId),
                BussinessCommon.getUser().getId(), listToUserId, comment), HttpStatus.OK);
    }

    @PostMapping("/transfer/{docId}")
    public ResponseEntity<?> transfer(@PathVariable Long docId, @PathParam(value = "toUserId") Long toUserId,
                                      @RequestParam(value = "delegateId", required = false) Long delegateId,
                                      @PathParam(value = "nodeId") Long nodeId, @RequestBody(required = false) String comment) {
        return new ResponseEntity<>(docOutService.transfer(docId, toUserId, delegateId, nodeId, comment),
                HttpStatus.OK);
    }

    @PostMapping("/finish/{docId}")
    public ResponseEntity<?> finish(@PathVariable Long docId,
                                    @RequestParam(value = "nodeId", required = false) Long nodeId,
                                    @RequestBody(required = false) String comment) {
        return new ResponseEntity<>(docOutService.finish(docId, nodeId, comment), HttpStatus.OK);
    }

    @PostMapping("/retake")
    public ResponseEntity<?> retake(@RequestParam(value = "docId") String docId,
                                    @RequestParam(value = "comment", required = false) String comment) {
        return new ResponseEntity<>(docOutService.retake(Long.parseLong(docId), comment), HttpStatus.OK);
    }

    @PostMapping("/retakeByStep")
    public ResponseEntity<?> retakeByStep(@RequestParam(value = "docId") Long docId,
                                          @RequestParam(required = false) String comment,
                                          @RequestParam(required = false) MultipartFile[] files) {
        return new ResponseEntity<>(docOutService.retakeByStep(docId, comment, files), HttpStatus.OK);
    }

    @PostMapping("/restore")
    public ResponseEntity<?> restore(@RequestParam(value = "docId") String docId,
                                     @RequestParam(value = "comment", required = false) String comment) {
        return new ResponseEntity<>(docOutService.restore(Long.parseLong(docId), comment), HttpStatus.OK);
    }

    @PostMapping("/reject")
    public ResponseEntity<?> reject(@RequestParam(value = "docId") String docId,
                                    @RequestParam(value = "comment", required = false) String comment) {
        return new ResponseEntity<>(docOutService.reject(Long.parseLong(docId), comment), HttpStatus.OK);
    }

    @PostMapping("/node-reject/do")
    public ResponseEntity<?> rejectByNodeId(@RequestParam Long docId,
                                            @RequestParam(required = false) String comment,
                                            @RequestParam Long userId,
                                            @RequestParam(required = false) Long nodeId,
                                            @RequestParam(required = false) Boolean delegate,
                                            @RequestParam(required = false) MultipartFile[] files) {
        return new ResponseEntity<>(docOutService.rejectByNodeId(docId, userId, nodeId, comment, delegate, files), HttpStatus.OK);
    }

    /**
     * show list node to choose node reject
     *
     * @param docId
     * @return
     */
    @GetMapping("/node-reject/list/{docId}")
    public ResponseEntity<?> nodeReject(@PathVariable Long docId) {
        return new ResponseEntity<>(docOutService.nodeReject(docId), HttpStatus.OK);
    }

    @PostMapping("/issued/{docId}")
    public ResponseEntity<?> issued(@PathVariable Long docId) {
        return new ResponseEntity<>(docOutService.issued(docId), HttpStatus.OK);
    }

    @PostMapping("/issuednew")
    public ResponseEntity<?> issued(@RequestBody IssuedDto input) {
        return new ResponseEntity<>(docOutService.issued(input), HttpStatus.OK);
    }

    @PostMapping("/issued")
    public ResponseEntity<?> issuedList(@PathParam(value = "listDocIds") Long[] listDocIds) {
        return new ResponseEntity<>(docOutService.issued(Arrays.asList(listDocIds)), HttpStatus.OK);
    }

    @GetMapping("/getListDocumentReceive")
    public ResponseEntity<?> getListDocumentReceive(@RequestParam Long docId) {
        return new ResponseEntity<>(docOutService.getListDocumentReceive(docId), HttpStatus.OK);
    }

    // Chuyển nhận để biết
    @PostMapping("/forward")
    public ResponseEntity<?> forward(@RequestBody ShowToKnowDto input) {
        return new ResponseEntity<>(docOutService.forward(input), HttpStatus.OK);
    }

    @PostMapping("/importDocBook")
    public ResponseEntity<?> importDocBook(@RequestParam Long docId, @RequestParam Long bookId,
                                           @RequestParam Long numberInBook, @RequestParam String numberOrSign) {
        return new ResponseEntity<>(docOutService.importDocBook(docId, bookId, numberInBook, numberOrSign),
                HttpStatus.OK);
    }

    @PostMapping("/importDocBookAndIssued")
    public ResponseEntity<?> importDocBookAndIssued(@RequestParam Long docId, @RequestParam Long bookId,
                                                    @RequestParam Long numberInBook, @RequestParam String numberOrSign) {
        return new ResponseEntity<>(docOutService.importDocBookAndIssued(docId, bookId, numberInBook, numberOrSign),
                HttpStatus.OK);
    }

    @GetMapping("/getListDocSign")
    public ResponseEntity<?> getListDocSign(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
                                            @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
                                            @RequestParam(required = false) Boolean important, @PathParam(value = "text") String text,
                                            @PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
                                            @PathParam(value = "docTypeId") Long docTypeId, @PathParam(value = "docFieldsId") Long docFieldsId,
                                            @PathParam(value = "startCreate") @DateTimeFormat(iso = ISO.DATE) Date startCreate,
                                            @PathParam(value = "endCreate") @DateTimeFormat(iso = ISO.DATE) Date endCreate,
                                            @RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
                                            @RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
        text = BussinessCommon.convert(text);
        numberOrSign = BussinessCommon.convert(numberOrSign);
        preview = BussinessCommon.convert(preview);
        startCreate = DateTimeUtils.handleSubmit(startCreate, Calendar.MILLISECOND, -1);
        endCreate = DateTimeUtils.handleSubmit(endCreate, Calendar.DAY_OF_MONTH, 1);
        Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
        Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
        return new ResponseEntity<>(docOutService.getListDraft(important, text, numberOrSign, preview, startCreate,
                endCreate, docTypeId, docFieldsId, pageable), HttpStatus.OK);
    }

    @GetMapping(value = "/getListDocOut")
    public ResponseEntity<?> getListDocOut(@PathParam(value = "page") Integer page) {
        return new ResponseEntity<>(docOutService.getListDocOut(page), HttpStatus.OK);
    }

    @GetMapping("/getListIssued")
    public ResponseEntity<?> getListIssued(@PathParam(value = "retaked") String retaked,
                                           @RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
                                           @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
                                           @RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
                                           @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
        Sort sort = Sort.by(direction, sortBy.field);
        Pageable pageable = PageRequest.of(page - 1, size, sort);
        String retakedL = retaked != null && retaked.length() > 0 ? retaked.toLowerCase() : null;
        return new ResponseEntity<>(docOutService.getListIssued(retakedL, pageable), HttpStatus.OK);
    }

    @GetMapping("/searchBasicIssued")
    public ResponseEntity<?> getListIssued(@PathParam(value = "retaked") String retaked,
                                           @PathParam(value = "text") String text,
                                           @RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
                                           @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
                                           @RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
                                           @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
        Sort sort = Sort.by(direction, sortBy.field);
        Pageable pageable = PageRequest.of(page - 1, size, sort);
        String retakedL = retaked != null && retaked.length() > 0 ? retaked.toLowerCase() : null;
        String textL = text != null && text.length() > 0 ? text.toLowerCase() : null;
        return new ResponseEntity<>(docOutService.getListIssued(retakedL, textL, pageable), HttpStatus.OK);
    }

    @GetMapping("/searchListIssued")
    public ResponseEntity<?> getListIssued(@PathParam(value = "retaked") String retaked,
                                           @PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
                                           @PathParam(value = "docTypeId") String docTypeId, @PathParam(value = "bookId") String bookId,
                                           @PathParam(value = "docFieldId") String docFieldsId,
                                           @PathParam(value = "startCreate") @DateTimeFormat(iso = ISO.DATE) Date startCreate,
                                           @PathParam(value = "endCreate") @DateTimeFormat(iso = ISO.DATE) Date endCreate,
                                           @PathParam(value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
                                           @PathParam(value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
                                           @PathParam(value = "orgCreateName") String orgCreateName,
                                           @PathParam(value = "personEnter") String personEnter,
                                           @RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
                                           @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
                                           @RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
                                           @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
        Sort sort = Sort.by(direction, sortBy.field);
        Pageable pageable = PageRequest.of(page - 1, size, sort);
        String retakedL = retaked != null && retaked.length() > 0 ? retaked.toLowerCase() : null;
        String numberOrSignL = numberOrSign != null && numberOrSign.length() > 0 ? numberOrSign.toLowerCase() : null;
        String previewL = preview != null && preview.length() > 0 ? preview.toLowerCase() : null;
        Long docTypeL = docTypeId != null && docTypeId.length() > 0 ? Long.parseLong(docTypeId) : null;
        Long docFieldsL = docFieldsId != null && docFieldsId.length() > 0 ? Long.parseLong(docFieldsId) : null;
        Long bookIdL = bookId != null && bookId.length() > 0 ? Long.parseLong(bookId) : null;
        String orgCreateNameL = orgCreateName != null && orgCreateName.length() > 0 ? orgCreateName.toLowerCase()
                : null;
        String personEnterL = personEnter != null && personEnter.length() > 0 ? personEnter : null;
        startCreate = DateTimeUtils.handleSubmit(startCreate, Calendar.MILLISECOND, -1);
        endCreate = DateTimeUtils.handleSubmit(endCreate, Calendar.DAY_OF_MONTH, 1);
        startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);
        endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);
        return new ResponseEntity<>(
                docOutService.getListIssued(retakedL, numberOrSignL, orgCreateNameL, personEnterL, previewL,
                        startCreate, endCreate, startIssued, endIssued, docTypeL, docFieldsL, bookIdL, pageable),
                HttpStatus.OK);
    }

    @GetMapping("/getListIssued/{type}")
    public ResponseEntity<?> getListIssued(@PathVariable Long type,
                                           @RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
                                           @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
                                           @RequestParam(required = false) Boolean important, @PathParam(value = "text") String text,
                                           @PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
                                           @PathParam(value = "docTypeId") Long docTypeId, @PathParam(value = "bookId") Long bookId,
                                           @PathParam(value = "docFieldsId") Long docFieldsId,
                                           @PathParam(value = "startCreate") @DateTimeFormat(iso = ISO.DATE) Date startCreate,
                                           @PathParam(value = "endCreate") @DateTimeFormat(iso = ISO.DATE) Date endCreate,
                                           @PathParam(value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
                                           @PathParam(value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
                                           @PathParam(value = "orgCreateName") String orgCreateName,
                                           @PathParam(value = "personEnter") String personEnter,
                                           @RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
                                           @RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
                                           @RequestParam(value = "isInternalDocument", defaultValue = "false") Boolean isInternalDocument) {
        text = BussinessCommon.convert(text);
        numberOrSign = BussinessCommon.convert(numberOrSign);
        preview = BussinessCommon.convert(preview);
        orgCreateName = BussinessCommon.convert(orgCreateName);
        personEnter = BussinessCommon.convert(personEnter);
        startCreate = DateTimeUtils.handleSubmit(startCreate, Calendar.MILLISECOND, -1);
        endCreate = DateTimeUtils.handleSubmit(endCreate, Calendar.DAY_OF_MONTH, 1);
        startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);
        endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);
        Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
        Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
        return new ResponseEntity<>(docOutService.getListIssued(important, type, text, numberOrSign, orgCreateName,
                personEnter, preview, startCreate, endCreate, startIssued, endIssued, docTypeId, docFieldsId, bookId, isInternalDocument,
                pageable), HttpStatus.OK);
    }

    @GetMapping("/knowable")
    public ResponseEntity<Page<KnowableDto>> knowable(
            @RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
            @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
            @RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
            @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
            @RequestParam(name = "read") Boolean doneTab) {
        Sort sort = JpaSort.unsafe(direction, sortBy.field);
        Pageable pageable = PageRequest.of(page - 1, size, sort);
        doneTab = doneTab == null ? false : doneTab;
        Page<KnowableDto> dopList = docOutService.knowable(doneTab, pageable);
        return new ResponseEntity<>(dopList, HttpStatus.OK);
    }

    @GetMapping("/quick-knowable")
    public ResponseEntity<Page<KnowableDto>> quickKnowable(
            @RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
            @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
            @RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
            @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
            @RequestParam(required = false) String q, @RequestParam(name = "read") Boolean doneTab) {
        q = StringUtils.handleSubmit(q);
        Sort sort = JpaSort.unsafe(direction, sortBy.field);
        Pageable pageable = PageRequest.of(page - 1, size, sort);
        doneTab = doneTab == null ? false : doneTab;
        Page<KnowableDto> dopList = docOutService.quickKnowable(q, doneTab, pageable);
        return new ResponseEntity<>(dopList, HttpStatus.OK);
    }

    @GetMapping("/search-knowable")
    public ResponseEntity<Page<KnowableDto>> searchKnowable(
            @RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) SortBy sortBy,
            @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
            @RequestParam(defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
            @RequestParam(required = false) Boolean important,
            @RequestParam(name = "preview", required = false) String preview,
            @RequestParam(name = "sign", required = false) String numberOrSign,
            @RequestParam(name = "docTypeId", required = false) Long docTypeId,
            @RequestParam(name = "docFieldId", required = false) Long docFieldId,
            @RequestParam(name = "orgName", required = false) String orgCreateName,
            @RequestParam(name = "handleType", required = false) HandleTypeEnum handleType,
            @RequestParam(name = "userEnter", required = false) String userEnter,
            @RequestParam(name = "startDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date startDate,
            @RequestParam(name = "endDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date endDate,
            @RequestParam(name = "startIssued", required = false) @DateTimeFormat(iso = ISO.DATE) Date startIssued,
            @RequestParam(name = "endIssued", required = false) @DateTimeFormat(iso = ISO.DATE) Date endIssued,
            @RequestParam(defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page,
            @RequestParam(name = "read") Boolean doneTab) {

        preview = StringUtils.handleSubmit(preview);
        numberOrSign = StringUtils.handleSubmit(numberOrSign);
        orgCreateName = StringUtils.handleSubmit(orgCreateName);
        userEnter = StringUtils.handleSubmit(userEnter);
        startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, -1);
        endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1);
        startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);
        endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);

        Sort sort = JpaSort.unsafe(direction, sortBy.field);
        Pageable pageable = PageRequest.of(page - 1, size, sort);
        doneTab = doneTab == null ? false : doneTab;
        Page<KnowableDto> dopList = docOutService.searchKnowable(important, preview, numberOrSign, docTypeId,
                docFieldId, orgCreateName, userEnter, startDate, endDate, startIssued, endIssued, doneTab, handleType,
                pageable);
        return new ResponseEntity<>(dopList, HttpStatus.OK);
    }

    @GetMapping("/getListAll")
    public ResponseEntity<?> getListAll(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
                                        @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
                                        @RequestParam(required = false) Boolean important, @PathParam(value = "text") String text,
                                        @PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
                                        @PathParam(value = "docTypeId") Long docTypeId, @PathParam(value = "bookId") Long bookId,
                                        @PathParam(value = "docFieldsId") Long docFieldsId, @PathParam(value = "docStatus") DocumentStatusEnum docStatus,
                                        @PathParam(value = "startCreate") @DateTimeFormat(iso = ISO.DATE) Date startCreate,
                                        @PathParam(value = "endCreate") @DateTimeFormat(iso = ISO.DATE) Date endCreate,
                                        @PathParam(value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
                                        @PathParam(value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
                                        @PathParam(value = "orgCreateName") String orgCreateName,
                                        @PathParam(value = "orgReceiveId") Long orgReceiveId,
                                        @PathParam(value = "personEnter") String personEnter,
                                        @RequestParam(value = "outsideReceive", required = false) String outsideReceive,
                                        @RequestParam(value = "size", defaultValue = Constant.DEFAULT_PAGE_SIZE) int size,
                                        @RequestParam(value = "page", defaultValue = Constant.DEFAULT_PAGE_NUMBER) int page) {
        text = BussinessCommon.convert(text);
        numberOrSign = BussinessCommon.convert(numberOrSign);
        preview = BussinessCommon.convert(preview);
        orgCreateName = BussinessCommon.convert(orgCreateName);
        personEnter = BussinessCommon.convert(personEnter);
        startCreate = DateTimeUtils.handleSubmit(startCreate, Calendar.MILLISECOND, -1);
        endCreate = DateTimeUtils.handleSubmit(endCreate, Calendar.DAY_OF_MONTH, 1);
        startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);
        endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);
        outsideReceive = BussinessCommon.convert(outsideReceive);
        Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
        Pageable pageable = BussinessCommon.castToPageable(page, sort, size);
        return new ResponseEntity<>(docOutService.getListAll(text, outsideReceive, important, numberOrSign,
                orgCreateName, orgReceiveId, personEnter, preview, startCreate, endCreate, startIssued, endIssued, docTypeId,
                docFieldsId, bookId, docStatus, pageable), HttpStatus.OK);
    }

    @GetMapping("/exportExcel")
    public ResponseEntity<?> exportExcel(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
                                         @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
                                         @RequestParam(required = false) Boolean important, @PathParam(value = "text") String text,
                                         @PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
                                         @PathParam(value = "docTypeId") Long docTypeId, @PathParam(value = "bookId") Long bookId,
                                         @PathParam(value = "docFieldsId") Long docFieldsId,
                                         @PathParam(value = "status") DocumentStatusEnum status,
                                         @PathParam(value = "startCreate") @DateTimeFormat(iso = ISO.DATE) Date startCreate,
                                         @PathParam(value = "endCreate") @DateTimeFormat(iso = ISO.DATE) Date endCreate,
                                         @PathParam(value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
                                         @PathParam(value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
                                         @PathParam(value = "orgCreateName") String orgCreateName,
                                         @PathParam(value = "orgReceiveId") Long orgReceiveId,
                                         @PathParam(value = "personEnter") String personEnter,
                                         @RequestParam(value = "outsideReceive", required = false) String outsideReceive,
                                         @RequestParam(required = false) Boolean configFields) {
        text = BussinessCommon.convert(text);
        numberOrSign = BussinessCommon.convert(numberOrSign);
        preview = BussinessCommon.convert(preview);
        orgCreateName = BussinessCommon.convert(orgCreateName);
        personEnter = BussinessCommon.convert(personEnter);
        startCreate = DateTimeUtils.handleSubmit(startCreate, Calendar.MILLISECOND, -1);
        endCreate = DateTimeUtils.handleSubmit(endCreate, Calendar.DAY_OF_MONTH, 1);
        startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);
        endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);
        outsideReceive = BussinessCommon.convert(outsideReceive);
        Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));
        if (Boolean.TRUE.equals(configFields)) {
            return new ResponseEntity<>(docOutService.exportExcelByConfig(text, outsideReceive, important, numberOrSign,
                    orgCreateName, personEnter, preview, startCreate, endCreate, startIssued, endIssued, docTypeId,
                    docFieldsId, bookId, orgReceiveId, status), HttpStatus.OK);
        }

        return new ResponseEntity<>(
                docOutService.exportExcel(text, outsideReceive, important, numberOrSign, orgCreateName, personEnter,
                        preview, startCreate, endCreate, startIssued, endIssued, docTypeId, docFieldsId, bookId, orgReceiveId, status),
                HttpStatus.OK);
    }

    @GetMapping("/exportExcelOut")
    public ResponseEntity<?> exportExcelOut(@RequestParam(defaultValue = Constant.DEFAULT_SORT_BY) String sortBy,
                                            @RequestParam(defaultValue = Constant.DEFAULT_DIRECTION) Direction direction,
                                            @PathParam(value = "numberOrSign") String numberOrSign, @PathParam(value = "preview") String preview,
                                            @PathParam(value = "bookId") Long bookId,
                                            @PathParam(value = "securityId") Long securityId,
                                            @PathParam(value = "urgentId") Long urgentId,
                                            @PathParam(value = "orgIssuedId") Long orgIssuedId,
                                            @PathParam(value = "docType") DocumentTypeEnum docType,
                                            @PathParam(value = "status") DocumentStatusEnum status,
                                            @PathParam(value = "docTypeId") Long docTypeId,
                                            @PathParam(value = "docFieldsId") Long docFieldsId,
                                            @PathParam(value = "startCreate") @DateTimeFormat(iso = ISO.DATE) Date startCreate,
                                            @PathParam(value = "endCreate") @DateTimeFormat(iso = ISO.DATE) Date endCreate,
//										 @PathParam(value = "startIssued") @DateTimeFormat(iso = ISO.DATE) Date startIssued,
//										 @PathParam(value = "endIssued") @DateTimeFormat(iso = ISO.DATE) Date endIssued,
//										 @PathParam(value = "orgCreateName") String orgCreateName,
//										 @PathParam(value = "personEnter") String personEnter,
//										 @RequestParam(value = "outsideReceive", required = false) String outsideReceive,
                                            @RequestParam(required = false) Boolean configFields) {
        numberOrSign = BussinessCommon.convert(numberOrSign);
        preview = BussinessCommon.convert(preview);
//		orgCreateName = BussinessCommon.convert(orgCreateName);
//		personEnter = BussinessCommon.convert(personEnter);
        startCreate = DateTimeUtils.handleSubmit(startCreate, Calendar.MILLISECOND, -1);
        endCreate = DateTimeUtils.handleSubmit(endCreate, Calendar.DAY_OF_MONTH, 1);
//		startIssued = DateTimeUtils.handleSubmit(startIssued, Calendar.MILLISECOND, -1);
//		endIssued = DateTimeUtils.handleSubmit(endIssued, Calendar.DAY_OF_MONTH, 1);
//		outsideReceive = BussinessCommon.convert(outsideReceive);
        Sort sort = JpaSort.unsafe(direction, SortBy.getEnum(sortBy));

        return new ResponseEntity<>(
                docOutService.exportExcelOut(numberOrSign,
                        preview, startCreate, endCreate, docTypeId, docFieldsId, docType, status, bookId, urgentId, securityId, orgIssuedId),
                HttpStatus.OK);
    }

    @GetMapping("/report")
    public ResponseEntity<ReportDto> report() {
        return new ResponseEntity<>(docOutService.report(), HttpStatus.OK);
    }

    @GetMapping("/fullReport")
    public ResponseEntity<List<ProcessByMonth>> fullReport(@RequestParam GroupBy groupBy,
                                                           @RequestParam(name = "startDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date startDate,
                                                           @RequestParam(name = "endDate", required = false) @DateTimeFormat(iso = ISO.DATE) Date endDate) {
        startDate = DateTimeUtils.handleSubmit(startDate, Calendar.MILLISECOND, -1);
        endDate = DateTimeUtils.handleSubmit(endDate, Calendar.DAY_OF_MONTH, 1);
        return new ResponseEntity<>(docOutService.fullReport(startDate, endDate, groupBy.month), HttpStatus.OK);
    }

    @PostMapping("/find_all/{page}")
    public ResponseEntity<Page<FindDocDto>> findAll(@PathVariable Integer page, @RequestBody FindDocDto dto) {
        return new ResponseEntity<>(docOutService.findAll(dto, page), HttpStatus.OK);
    }

    @PostMapping("/find_all_doc/{page}")
    public ResponseEntity<Page<FindDocDto>> findAllDoc(@PathVariable Integer page, @RequestBody FindDocDto dto) {
        return new ResponseEntity<>(docOutService.findAllDoc(dto, page), HttpStatus.OK);
    }

    @GetMapping("/checkNumberOrSign")
    public ResponseEntity<?> checkNumberOrSign(@RequestParam String numberOrSign, @RequestParam Long bookId) {
        return new ResponseEntity<>(docOutService.checkNumberOrSign(numberOrSign, bookId), HttpStatus.OK);
    }

    @GetMapping("/checkAction")
    public ResponseEntity<?> checkAction(@RequestParam Long docId,
                                         @RequestParam(required = false, defaultValue = "DA_XU_LY") Tab tab,
                                         @RequestParam(required = false, defaultValue = "FALSE") boolean isDelegate) {
        return new ResponseEntity<>(docOutService.checkAction(docId, tab, isDelegate), HttpStatus.OK);
    }

    @GetMapping("/report_doc_by_type")
    public ResponseEntity<ReportDocByTypeDto> reportDocByType() {
        return new ResponseEntity<>(docOutProcess.reportDocByType(), HttpStatus.OK);
    }

    @PostMapping("/track_download/{fileName:.+}")
    public ResponseEntity<?> trackDownload(@PathVariable String fileName) {
        return new ResponseEntity<>(docOutTracking.trackDownload(fileName), HttpStatus.OK);
    }

    @GetMapping("/addSignUser")
    public ResponseEntity<?> addSignUser(@RequestParam Long docId, @RequestParam Long userId) {
        return new ResponseEntity<>(docOutService.addSignUser(docId, userId), HttpStatus.OK);
    }

    @GetMapping("/out-side-receive/list")
    public ResponseEntity<List<String>> getOutsideReceiveList() {
        return new ResponseEntity<>(docOutService.getOutsideReceiveList(), HttpStatus.OK);
    }

    @PostMapping("/read")
    public ResponseEntity<?> setRead(@RequestParam List<Long> docIds) {
        return new ResponseEntity<>(docOutService.setRead(docIds, BussinessCommon.getUserId()), HttpStatus.OK);
    }

    @PostMapping("/forward/{docId}")
    public ResponseEntity<?> forward(@PathVariable Long docId,
                                     @RequestParam(value = "main", required = false) Long main,
                                     @RequestParam(value = "support", required = false) List<Long> support,
                                     @RequestParam(value = "show", required = false) List<Long> show) {
        return new ResponseEntity<>(docOutService.forwardDoc(docId, main, support, show), HttpStatus.OK);
    }

    @GetMapping("/forward/users")
    public ResponseEntity<?> forward() {
        return new ResponseEntity<>(docOutService.getUserForwardList(), HttpStatus.OK);
    }

    @GetMapping("/forward/list/{docId}")
    public ResponseEntity<?> forward(@PathVariable Long docId) {
        return new ResponseEntity<>(docOutService.getReceivedUser(docId), HttpStatus.OK);
    }

    @PostMapping("/forward/finish")
    public ResponseEntity<?> finishRead(@RequestParam Long docId, @RequestParam(required = false) String comment) {
        return new ResponseEntity<>(docOutService.finishRead(docId, BussinessCommon.getUserId(), comment),
                HttpStatus.OK);
    }

    @PostMapping("/forward/unread")
    public ResponseEntity<?> unFinishRead(@RequestParam Long docId, @RequestParam(required = false) String comment) {
        return new ResponseEntity<>(docOutService.unfinishRead(docId, BussinessCommon.getUserId(), comment), HttpStatus.OK);
    }

    @PostMapping("/forward/additional/{docId}")
    public ResponseEntity<?> forwardAdditional(@PathVariable Long docId,
                                               @RequestParam(value = "main", required = false) Long main,
                                               @RequestParam(value = "support", required = false) List<Long> support,
                                               @RequestParam(value = "show", required = false) List<Long> show,
                                               @RequestParam(required = false) String comment) {
        return new ResponseEntity<>(docOutService.forwarAdditional(docId, main, support, show, comment), HttpStatus.OK);
    }

    @PostMapping(value = "/suggest/user")
    public ResponseEntity<List<SignerDto>> suggestUser(@RequestParam(required = false) String text) {
        text = BussinessCommon.convert(text);
        return new ResponseEntity<>(docOutService.getSignerIdByCreator(text), HttpStatus.OK);
    }

    @PostMapping("/issued/print-data")
    public ResponseEntity<?> print(@RequestParam Long docId, @RequestParam(required = false) String numberOrSign,
                                   @RequestParam(required = false) Date issuedDate) {
        docOutService.beforePrintDocIssuedData(docId, numberOrSign, issuedDate);
        return new ResponseEntity<>(true, HttpStatus.OK);
    }
}
