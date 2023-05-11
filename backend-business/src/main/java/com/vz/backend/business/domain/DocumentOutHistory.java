package com.vz.backend.business.domain;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.config.DocumentOutActionEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;
import lombok.*;

import javax.persistence.*;

@Entity
@Table(name = "DOCUMENT_OUT_HISTORY", schema = "vz")
@Getter
@Setter
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class DocumentOutHistory extends BaseModel {

    @Column(name = "doc_out_id")
    private Long docOutId;

    @JsonIgnore
    @OneToOne
    @JoinColumn(name = "doc_out_id", insertable = false, updatable = false)
    private DocumentOut documentOut;

    @Column(name = "action")
    @Enumerated(EnumType.STRING)
    private DocumentOutActionEnum action;

    @Column(name = "book_name")
    private String bookName;

    @Column(name = "number_in_book")
    private Long numberInBook;

    @Column(name = "number_sign")
    private String numberOrSign;

    @Column(name = "doc_type")
    private String docType;

    @Column(columnDefinition = "TEXT", name = "preview")
    private String preview;

    @Column(name = "internal_receivers", length = 5000)
    private String internalReceivers;

    @Column(name = "internal_receivers_description")
    private String internalReceiversDescription;

    @Column(name = "external_receivers")
    private String externalReceivers;

    @Column(name = "urgent")
    private String urgent;

    @Column(name = "security")
    private String security;

    @Column(name = "list_signers_name")
    private String listSignersName;

    @Column(name = "count_issued")
    private Integer countIssued;

    @Column(name = "org_create_name")
    private String orgCreateName;

    @Column(name = "user_id")
    private Long userId;

    @ManyToOne(fetch = FetchType.EAGER)
    @JoinColumn(name = "user_id", insertable = false, updatable = false)
    private User user;

    @Column(name = "user_create_name")
    private String userCreateName;


}
