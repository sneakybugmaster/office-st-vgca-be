package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "TASK_RELATED", schema = "vz", uniqueConstraints = { @UniqueConstraint(columnNames = { "client_id", "task_id", "task_related_id" }) })
@AllArgsConstructor
@NoArgsConstructor
@Data
public class TaskRelated extends BaseModel {
	@Column(name = "task_id")
	private Long taskId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	@JoinColumn(name = "task_id", updatable = false, insertable = false)
	private Task task;
	
	@Column(name = "task_related_id")
	private Long taskRelatedId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY, optional = false)
	@JoinColumn(name = "task_related_id", updatable = false, insertable = false)
	private Task taskRelated;
	
	@Override
	public void valids() {
		BussinessCommon.require("Công việc liên quan", this.taskRelatedId);
	}
	
	public String getNamRelated() {
		return this.taskRelated != null ? this.taskRelated.getTaskName() : "";
	}
	
	public String getUserAssignName() {
		return this.taskRelated != null ? this.taskRelated.getUserAssign().getFullName() : "";
	}
}
