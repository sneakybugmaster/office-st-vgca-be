package com.vz.backend.business.service;

import com.vz.backend.business.domain.DocumentOut;
import com.vz.backend.business.domain.DocumentOutHistory;
import com.vz.backend.business.domain.OutsideReceiveDocument;
import com.vz.backend.business.dto.DocumentOutHistoryDto;
import com.vz.backend.business.repository.IDocumentOutHistoryRepository;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentOutActionEnum;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.service.UserService;
import com.vz.backend.util.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.Comparator;
import java.util.Optional;
import java.util.stream.Collectors;

@Service
public class DocumentOutHistoryService {
    @Autowired
    private IDocumentOutHistoryRepository documentOutHistoryRepository;

    @Autowired
    private UserService userService;


    public DocumentOutHistory save(DocumentOut documentOut, DocumentOutActionEnum documentOutActionEnum) {
        String internalReceivers = null;
        String externalReceivers = null;
        if (documentOut.getListReceive() != null) {
            internalReceivers = StringUtils.listStringToDelimitedString(documentOut.getListReceive().stream().sorted(Comparator.comparing(r -> r.getType().equals("ORG") ? "" : r.getType())).map(r -> r.getType().equals("USER") ? (r.getFullName() != null ? r.getFullName() : r.getName()) : (r.getOrgName() != null ? r.getOrgName() : r.getName())).collect(Collectors.toList()), ", ");
        }

        if (documentOut.getOutsideReceives() != null) {
            externalReceivers = StringUtils.listStringToDelimitedString(documentOut.getOutsideReceives().stream().map(OutsideReceiveDocument::getAddress).collect(Collectors.toList()), ", ");
        }

        User currentUser = BussinessCommon.getUser();
        Optional<User> createUserOptional = userService.findById(documentOut.getCreateBy());

        DocumentOutHistory documentOutHistory = DocumentOutHistory.builder()
                .bookName(documentOut.getBookName())
                .docType(documentOut.getDocTypeName())
                .security(documentOut.getDocSecurityName())
                .urgent(documentOut.getDocUrgentName())
                .listSignersName(documentOut.getListSignersName())
                .action(documentOutActionEnum)
                .preview(documentOut.getPreview())
                .numberInBook(documentOut.getNumberInBook())
                .numberOrSign(documentOut.getNumberOrSign())
                .orgCreateName(documentOut.getOrgCreateName())
                .docOutId(documentOut.getId())
                .countIssued(documentOut.getCountIssued())
                .internalReceiversDescription(documentOut.getInternalReceiversDescription())
                .internalReceivers(StringUtils.trimRepeatingSpaces(internalReceivers))
                .externalReceivers(StringUtils.trimRepeatingSpaces(externalReceivers))
                .listSignersName(StringUtils.trimRepeatingSpaces(documentOut.getListSignersName()))
                .userId(currentUser.getId())
                .userCreateName(createUserOptional.map(User::getFullName).orElse(null))
                .build();

        return documentOutHistoryRepository.save(documentOutHistory);

    }

    public Page<DocumentOutHistoryDto> getHistoryByDocOutId(Long docOutId, Pageable pageable, Long clientId) {
        return documentOutHistoryRepository.findByDocOutIdAndClientIdOrderByCreateDateDesc(docOutId, pageable, clientId);
    }
}
