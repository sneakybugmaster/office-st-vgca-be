package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.domain.BaseModel;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "DOCUMENT_OUT_TASK", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "doc_out_id", "task_id" }) })
@Getter
@Setter
@NoArgsConstructor
public class DocumentOutTask extends BaseModel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "doc_out_id")
	private Long docOutId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "doc_out_id", insertable = false, updatable = false)
	private DocumentOut docOut;
	
	@Column(name = "task_id")
	private Long taskId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "task_id", updatable = false, insertable = false)
	private Task task;
	
	public DocumentOutTask(Long docOutId, Long taskId) {
		super();
		this.docOutId = docOutId;
		this.taskId = taskId;
	}
}
