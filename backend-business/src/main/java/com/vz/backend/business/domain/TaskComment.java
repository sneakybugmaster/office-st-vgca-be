package com.vz.backend.business.domain;

import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "TASK_COMMENT", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TaskComment extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "user_id")
	private Long userId;
	@Column(name = "user_full_name")
	private String userFullName;

	@Column(name = "user_position")
	private String userPosition;

	@Column(name = "comment", columnDefinition = "TEXT")
	private String comment;
	
	@Column(name = "isToken")
	private Boolean isToken;

	@Column(name = "comment_date")
	private Date commentDate;

	@Column(name = "task_id")
	private Long taskId;

	@Transient
	private List<TaskAttachment> attachments;

	@PrePersist
	@PreUpdate
	public void addDate() {
		this.commentDate = new Date();
	}

	public TaskComment(String comment, Long taskId) {
		User user = BussinessCommon.getUser();
		this.userId = user.getId();
		this.userFullName = user.getFullName();
		this.userPosition = user.getPositionModel() != null ? user.getPositionModel().getName() : "";
		this.comment = comment;
		this.taskId = taskId;
	}
}
