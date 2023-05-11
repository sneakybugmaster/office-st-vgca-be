package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "TASK_DOCUMENT", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TaskDocument extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "task_id")
	private Long taskId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "task_id", updatable = false, insertable = false)
	private Task task;
	
	@Column(name = "doc_id")
	private Long docId;

	@Transient
	private Documents documentIn;

	@Transient
	private DocumentOut documentOut;

	@Column(name = "type_document")
	private Boolean typeDocument; // true: Văn bản đến, false: văn bản đi

}
