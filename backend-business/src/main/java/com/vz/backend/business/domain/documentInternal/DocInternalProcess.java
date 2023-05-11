package com.vz.backend.business.domain.documentInternal;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.*;

@Entity
@Table(name = "DOC_INTERNAL_PROCESS", schema = "vz")
@Getter
@Setter
@NoArgsConstructor
public class DocInternalProcess extends BaseModel {

    @Column(name = "doc_id")
    private Long docId;
    @JsonIgnoreProperties("hibernateLazyInitializer")
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "doc_id", updatable = false, insertable = false)
    private DocumentInternal documentInternal;

    @Column(name = "to_user")
    private Long toUser;
    @JsonIgnore
    @JsonIgnoreProperties("hibernateLazyInitializer")
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "to_user", updatable = false, insertable = false)
    private User toUsers;

    @Column(name = "fr_user")
    private Long frUser;
    @JsonIgnoreProperties("hibernateLazyInitializer")
    @JsonIgnore
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "fr_user", updatable = false, insertable = false)
    private User frUsers;

    @Enumerated(EnumType.STRING)
    @Column(name = "handle_type")
    private HandleTypeEnum handleType;

    @Column(name = "step")
    private Integer step;

    @Column(name = "node")
    private Long node;

    @Column(name = "progress")
    private Integer progress;

    @Column(name = "comment")
    private String comment;

    @Column(name = "transferStep")
    private Integer transferStep;

    @Column(name = "pre_node")
    private Long preNode;

    @Column(name = "next_node")
    private Long nextNode;

    @Column(name = "transfer")
    private Boolean transfer;

    @Column(name = "end_task")
    private Boolean endTask;
}
