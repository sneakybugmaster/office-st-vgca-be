package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "TASK_HISTORY", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties({ "handler", "hibernateLazyInitializer", "updateDate", "createBy", "updateBy", "active",
		"clientId" })
public class TaskHistory extends BaseModel {

	@Column(name = "task_id", nullable = false)
	private Long taskId;

	@Column(columnDefinition = "TEXT", name = "task_name")
	private String taskName;

	@Column(columnDefinition = "TEXT", name = "description")
	private String description;

	public void valids() {
//		BussinessCommon.require("Công việc", this.taskId);
		BussinessCommon.require("Tên công việc", this.taskName);
		BussinessCommon.validLengthData(this.taskName, "Tên công việc ", 1000);
		BussinessCommon.validLengthData(this.description, "Mô tả công việc ", 2000);
	}

	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "create_by", insertable = false, updatable = false)
	private User user;

	public String getCreator() {
		return this.user == null ? "" : this.user.getFullName();
	}

	public TaskHistory(Task task) {
		this.taskId = task.getId();
		this.taskName = task.getTaskName();
		this.description = task.getDescription();
	}
}
