package com.vz.backend.business.dto;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.vz.backend.business.domain.Attachment;
import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentReceive;
import com.vz.backend.business.domain.Documents;
import com.vz.backend.core.config.DocumentStatusEnum;
import lombok.Getter;
import lombok.Setter;

import java.util.Date;
import java.util.List;

@Setter
@Getter
public class DocumentDetailDto {
    private Date deadline;
    private Date dateArrival;
    private Date dateIssued;
    private Date receivedDate;
    private List<Attachment> attachments;
    @JsonInclude(JsonInclude.Include.NON_NULL)
    private List<DocumentReceive> documentReceivers;
    private DocumentStatusEnum status;
    private String preview;
    private String numberArrivalStr;
    private String placeSend;
    private String numberOrSign;
    private Long id;
    private Boolean confidential;
    private String directorGuidance;
    private Boolean isInternalDocument;

    public void setFromDocuments(Documents document, DocumentOut documentOut) {
        this.id = document.getId();
        this.deadline = document.getDeadline();
        this.dateArrival = document.getDateArrival();
        this.dateIssued = document.getDateIssued();
        this.receivedDate = document.getReceivedDate();
        this.attachments = document.getAttachments();
        this.status = document.getStatus();
        this.preview = document.getPreview();
        this.numberArrivalStr = document.getNumberArrivalStr();
        this.placeSend = document.getPlaceSend();
        this.numberOrSign = document.getNumberOrSign();
        this.confidential = document.getConfidential();
        this.isInternalDocument = document.getIsInternalDocument();
        if (documentOut != null) {
            this.directorGuidance = documentOut.getDirectorGuidance();
            this.documentReceivers = documentOut.getListReceive();
        }

    }
}
