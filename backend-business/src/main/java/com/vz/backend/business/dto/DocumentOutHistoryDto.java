package com.vz.backend.business.dto;

import com.vz.backend.business.domain.DocumentOutHistory;
import lombok.Getter;
import lombok.Setter;

import java.util.Date;

@Getter
@Setter
public class DocumentOutHistoryDto {

    private Long historyId;

    private Long docOutId;

    private String action;

    private String bookName;

    private Long numberInBook;

    private String numberOrSign;

    private String docType;

    private String preview;

    private String internalReceivers;

    private String internalReceiversDescription;

    private String externalReceivers;

    private String urgent;

    private String security;

    private String listSignersName;

    private Integer countIssued;

    private String orgCreateName;

    private Long userId;

    private String userFullName;

    private String userPosition;

    private String userCreateName;

    private String orgName;

    private Date createDate;

    public DocumentOutHistoryDto(DocumentOutHistory documentOutHistory) {
        this.action = documentOutHistory.getAction().getName();
        this.historyId = documentOutHistory.getId();
        this.docOutId = documentOutHistory.getDocOutId();
        this.bookName = documentOutHistory.getBookName();
        this.numberInBook = documentOutHistory.getNumberInBook();
        this.numberOrSign = documentOutHistory.getNumberOrSign();
        this.docType = documentOutHistory.getDocType();
        this.preview = documentOutHistory.getPreview();
        this.internalReceivers = documentOutHistory.getInternalReceivers();
        this.internalReceiversDescription = documentOutHistory.getInternalReceiversDescription();
        this.externalReceivers = documentOutHistory.getExternalReceivers();
        this.urgent = documentOutHistory.getUrgent();
        this.security = documentOutHistory.getSecurity();
        this.listSignersName = documentOutHistory.getListSignersName();
        this.countIssued = documentOutHistory.getCountIssued();
        this.orgCreateName = documentOutHistory.getOrgCreateName();
        this.userId = documentOutHistory.getUserId();
        this.createDate = documentOutHistory.getCreateDate();
        this.userCreateName = documentOutHistory.getUserCreateName();
        if (documentOutHistory.getUser() != null) {
            this.userFullName = documentOutHistory.getUser().getFullName();
            this.userPosition = documentOutHistory.getUser().getPositionModel().getName();
            this.orgName = documentOutHistory.getUser().getOrgModel().getName();
        }
    }

}
