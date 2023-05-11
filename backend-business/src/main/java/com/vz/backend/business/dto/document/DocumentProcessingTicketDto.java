package com.vz.backend.business.dto.document;

import lombok.Builder;
import lombok.Data;

import java.util.ArrayList;
import java.util.List;

@Data
public class DocumentProcessingTicketDto {
    private String orgNameUpper;
    private String currentDate;
    private String numberArrival;
    private String iDate;
    private String iMonth;
    private String iYear;
    private String numberOrSign;
    private String placeSend;
    private String preview;
    private String position;
    private String orgName;
    private List<ProcessCommentDto> comments = new ArrayList<>();
    private List<ProcessingDto> processes = new ArrayList<>();

    @Data
    @Builder
    public static class ProcessingDto {
        private String senderPosition;
        private String senderOrgName;
        private String transferDateTime;
        private String receivers;
    }

    @Data
    @Builder
    public static class ProcessCommentDto {
        private String commenterPosition;
        private String commenterOrgName;
        private String commentDateTime;
        private String comment;
    }
}
