package com.vz.backend.business.controller;

import com.vz.backend.business.service.DocumentOutHistoryService;
import com.vz.backend.core.common.BussinessCommon;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/doc-out-history")
public class DocumentOutHistoryController {

    @Autowired
    private DocumentOutHistoryService documentOutHistoryService;

    @GetMapping("/get-by-docid/{docId}")
    public ResponseEntity<?> getHistoriesByDocId(@PathVariable Long docId,
                                                 @RequestParam(value = "page", defaultValue = "1") int page,
                                                 @RequestParam(value = "size", defaultValue = "10") int size) {
        Pageable pageable = PageRequest.of(page - 1, size, Sort.Direction.DESC, "createDate");
        return ResponseEntity.ok(documentOutHistoryService.getHistoryByDocOutId(docId, pageable, BussinessCommon.getClientId()));
    }
}
