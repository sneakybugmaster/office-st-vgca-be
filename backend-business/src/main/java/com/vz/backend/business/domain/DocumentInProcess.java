package com.vz.backend.business.domain;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.*;

import org.hibernate.annotations.NamedNativeQueries;
import org.hibernate.annotations.NamedNativeQuery;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.HandleTypeEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@SqlResultSetMapping(name = "documentStepResult", classes = {
		@ConstructorResult(targetClass = com.vz.backend.business.dto.KnowableDto.class, columns = {
				@ColumnResult(name = "id")
		}) 
})
@NamedNativeQueries(value = {
		@NamedNativeQuery(name = "Documents.step", query = com.vz.backend.business.util.Constant.PID_QUERY, resultSetMapping = "documentStepResult"), })
@Data
@NoArgsConstructor
@AllArgsConstructor
@Table(name = "SYS_DOCUMENT_IN_PROCESS", schema = "vz", indexes = {@Index(name = "DOC_IN_PROCESS_INX_SXX",columnList = "id,doc_id,to_user,fr_user,delegater_id")})
public class DocumentInProcess extends BaseModel {

	private static final long serialVersionUID = 1L;
	@Column(name = "doc_id")
	private Long docId;
	@JsonIgnoreProperties("hibernateLazyInitializer")
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_id", updatable = false, insertable = false)
	private Documents document;

	@Column(name = "org_name")
	private String orgName;

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

	@Enumerated(EnumType.STRING)
	@Column(name = "handle_status")
	private DocumentInHandleStatusEnum handleStatus;

	@Column(name = "step")
	private Integer step;

	@Column(name = "node")
	private Long node;

	@Column(name = "delegater_id")
	private Long delegaterId;
	@JsonIgnoreProperties("hibernateLazyInitializer")
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "delegater_id", updatable = false, insertable = false, nullable = true)
	private User delegater;

	@Column(name = "progress")
	private Integer progress;

	@Column(name = "comment")
	private String comment;

	@Column(name = "deadline")
	private Date deadline;
	
	@Column(name = "review")
	private Boolean review;
	
	@Column(name = "transferStep")
	private Integer transferStep;
	
	@Column(name = "pre_node")
	private Long preNode;
	
	/**
	 * Transfer with direction role
	 */
	@Column(name = "direction")
	private Boolean direction;
	
	@Column(name = "rq_review")
	private Boolean requestReview;
	
	/**
	 * Flag mark user close document
	 */
	@Column(name = "[close]")
	private Boolean close;
	
	/**
	 * Flag mark user close branch (include people's record that is transfered by current_record document
	 */
	@Column(name = "close_branch")
	private Boolean closeBranch;
	
	/**
	 * Flag mark user has transfered
	 */
	@Column(name = "transfer")
	private Boolean transfer;
	
	@Transient
	@JsonIgnore
	private List<DocumentInProcess> childs = new ArrayList<>();
	
	@Column(name = "next_node")
	private Long nextNode;

	@Column(name = "sign_date")
	private Date signDate; // Ngày ký khi hoàn thành xử lý
	
	/**
	 * Flag mark user is done process (record move from did tab to done tab in document list)
	 */
	@Column(name = "end_task")
	private Boolean endTask;
	
	public void set(DocumentInProcess old, int step, int maxTransferStep) {
		this.frUser = BussinessCommon.getUserId();
		this.docId = old.docId;
		this.toUser = old.toUser;
		this.delegaterId = old.delegaterId;
		this.node = old.node;
		this.preNode = old.preNode;
		this.transferStep = maxTransferStep;
		this.step = step;
		this.setOrgName(old.getOrgName());
		this.handleType = HandleTypeEnum.MAIN;
		this.handleStatus = DocumentInHandleStatusEnum.CHO_XU_LY;
	}
	
	@PrePersist
	@PreUpdate
	public void prePersit() {
		if(this.transferStep == null) this.transferStep = Constant.NO_STEP;
		if(this.preNode == null) this.preNode = Constant.START_NODE;
	}
	
	public Integer getTransferStep() {
		return this.transferStep == null ? 0 : this.transferStep;
	}
}
