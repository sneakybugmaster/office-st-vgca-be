package com.vz.backend.business.dto.document;

import com.vz.backend.business.config.ViewStatusEnum;
import com.vz.backend.business.domain.Documents;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;
import java.util.Date;

@NoArgsConstructor
@Data
@AllArgsConstructor
public class DocumentDto implements Serializable {
    private Long docId;
    private Date issuedDate;
    private String issuedOrg;
    private Date receivedDate;
    private Long incomingNum;
    private String preview;
    private String numberOrSign;
    private Date assignedDate;
    private Date deadLine;
    private String assigneeName;
    private String orgName;
    private Date handledDate;
    private String urgent;
    private String security;
    private ViewStatusEnum viewStatus;

    public DocumentDto(Documents d, String viewStatusString, Date deadLine) {
        this.docId = d.getId();
        this.incomingNum = d.getNumberArrival();
        this.preview = d.getPreview();
        this.numberOrSign = d.getNumberOrSign();
        this.receivedDate = d.getDateIssued();
        this.issuedDate = d.getDateArrival();
        this.issuedOrg = d.getPlaceSend();
        this.urgent = d.getUrgentName();
        this.security = d.getSecurityName();
        this.deadLine = deadLine;
        try {
            this.viewStatus = ViewStatusEnum.valueOf(viewStatusString);
        } catch (IllegalStateException ignored) {

        }
    }

}
