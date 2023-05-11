package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "WORD_EDITOR_PROCESS", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties({ "hibernateLazyInitializer" })
public class WordEditorProcess extends BaseModel {
	
	@Column(name = "fr_user")
	private Long frUserId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "fr_user", insertable = false, updatable = false)
	private User frUser;
	
	@Column(name = "to_user")
	private Long toUserId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "to_user", insertable = false, updatable = false)
	private User toUser;
	
	@Column(name = "node")
	private Long node;
	
	@Column(name = "we_id")
	private Long weId;
	
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "we_id", insertable = false, updatable = false)
	private WordEditor we;

	@Enumerated(EnumType.STRING)
	@Column(name = "handle_status")
	private DocumentInHandleStatusEnum handleStatus;
	
	private Integer step;
	
	private String content;

	public WordEditorProcess(Long weId, Long frUserId, Long toUserId, Long node, DocumentInHandleStatusEnum handleStatus, int step) {
		this.frUserId = frUserId;
		this.toUserId = toUserId;
		this.node = node;
		this.weId = weId;
		this.handleStatus = handleStatus;
		this.step = step;
	}
	
	@PrePersist
	public void prePersit() {
		//this.step = Constant.START_STEP;
		this.frUserId = BussinessCommon.getUserId();
		//this.node = Constant.START_NODE;
	}
}
