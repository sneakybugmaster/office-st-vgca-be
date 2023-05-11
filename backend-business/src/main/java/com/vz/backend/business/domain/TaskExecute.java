package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToOne;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Group;
import com.vz.backend.core.domain.Organization;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.exception.RestExceptionHandler;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @author DucND
 * @date May 29, 2020
 */
@Entity
@Table(name = "TASK_EXCUTE", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties({ "handler", "hibernateLazyInitializer", "updateDate", "createBy", "updateBy", "active",
		"clientId" })
public class TaskExecute extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "task_id")
	private Long taskId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "task_id", insertable = false, updatable = false)
	private Task task;

	@Column(name = "user_id")
	private Long userId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "user_id", insertable = false, updatable = false)
	private User user;

	@Column(name = "group_id")
	private Long groupId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "group_id", insertable = false, updatable = false)
	private Group group;

	@Column(name = "type")
	private Integer type; // 0.user, 1.group, 2.org

	@Column(name = "is_excute")
	private Boolean isExcute;

	@Column(name = "is_combination")
	private Boolean isCombination;

	@Column(columnDefinition = "TEXT", name = "description")
	private String description;

	@Column(name = "important")
	private Boolean important;

	@Column(name = "status")
	private Integer status; // 0.mới giao, 1.đang thực hiện, 2.từ chối, 3.hoàn thành(chờ đánh giá), 4.close

	@Column(name = "progress")
	private Integer progress;

	@Column(name = "comment")
	private String comment;

	@Column(name = "[read]")
	private Boolean read;

	// Giao cho phòng ban -> về trưởng phòng
	@Column(name = "org_id")
	private Long orgId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "org_id", insertable = false, updatable = false)
	private Organization org;

	private Integer step;

	// History task info
	@Column(name = "task_history_id")
	private Long taskHistoryId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "task_history_id", insertable = false, updatable = false)
	private TaskHistory taskHistory;

	@Column(name = "fr_user_id")
	private Long frUserId;
	@ManyToOne
	@JoinColumn(name = "fr_user_id", insertable = false, updatable = false)
	private User frUser;

	private Long node;

	/**
	 * Chờ đánh giá
	 */
	private Boolean reviewRequired;

	/**
	 * Flag mark user close task
	 */
	@Column(name = "[close]")
	private Boolean close;

	private Long nextNode;
	
	@PrePersist
	@PreUpdate
	public void prePersit() {
		if (this.important == null) {
			this.important = false;
		}

		if (this.read == null) {
			this.read = false;
		}

		if (this.type == null) {
			this.type = com.vz.backend.core.config.Constant.USER;
		}

		if (this.isCombination == null) {
			this.isCombination = false;
		}

		if (this.isExcute == null) {
			this.isExcute = false;
		}

		if (this.node == null) {
			this.node = Constant.START_NODE;
		}

		if (this.step == null) {
			this.step = Constant.START_STEP;
		}

		if (this.frUserId == null) {
			this.frUserId = BussinessCommon.getUserId();
		}
	}

	public void set(TaskExecute taskExe) {
		this.setIsExcute(taskExe.getIsExcute());
		this.setIsCombination(taskExe.getIsCombination());
		this.setDescription(taskExe.getDescription());
	}

	@Override
	public void valids() {
		BussinessCommon.validLengthData(this.description, "Mô tả công việc ", 1000);

		// valid data by type
		validType(this.type);
		if (this.type.intValue() == com.vz.backend.core.config.Constant.ORG) {
			BussinessCommon.require("Tổ chức", this.orgId);
		}

		if (this.type.intValue() == com.vz.backend.core.config.Constant.GROUP) {
			BussinessCommon.require("Nhóm", this.groupId);
		}

		if (this.type.intValue() == com.vz.backend.core.config.Constant.USER) {
			BussinessCommon.require("Người thực hiện", this.userId);
		}
	}

	/**
	 * Valid type :0.user, 1.group, 2.org
	 *
	 * @param type
	 */
	private void validType(Integer type) {
		if (type == null || type.intValue() < 0 || type.intValue() > 2)
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);
	}

	public void set(Long taskId, Long userId, Integer step, Long node, Integer status) {
		this.taskId = taskId;
		this.status = status;
		this.step = step;
		this.frUserId = userId;
		this.node = node;
	}

	public Integer getProgress() {
		if (this.status == com.vz.backend.business.util.Constant.TASK_EXCUTE_STATUS_COMPLETE
				|| this.status == com.vz.backend.business.util.Constant.TASK_EXCUTE_STATUS_CLOSE)
			return 100;
		return progress;
	}

	/**
	 * For migrate data when apply thread to task
	 * @param task
	 */
	public TaskExecute (Task task) {
		this.frUserId = task.getCreateBy();
		this.userId = task.getCreateBy();
		this.type = com.vz.backend.core.config.Constant.USER;
		this.setIsExcute(true);
		this.step = Constant.START_STEP;
		this.node = Constant.START_NODE;
		this.setCreateBy(task.getCreateBy());
		this.setCreateDate(task.getCreateDate());;
		this.setUpdateDate(task.getUpdateDate());
		this.taskId = task.getId();
		this.status = task.getStatus();
		if(task.getStatus() == 4 || this.getStatus() == 2) {
			this.status = task.getStatus();
		} else {
			this.status = 3;
		}
	}
}
