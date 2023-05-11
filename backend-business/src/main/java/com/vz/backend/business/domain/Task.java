package com.vz.backend.business.domain;

import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.stream.Collectors;

import javax.persistence.Column;
import javax.persistence.ColumnResult;
import javax.persistence.ConstructorResult;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.SqlResultSetMapping;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.vz.backend.core.domain.Organization;
import org.hibernate.annotations.NamedNativeQueries;
import org.hibernate.annotations.NamedNativeQuery;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.dto.DocumentBasicDto;
import com.vz.backend.business.util.Constant;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Category;
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
@SqlResultSetMapping(name = "commonUnionResult", classes = {
		@ConstructorResult(targetClass = com.vz.backend.business.dto.ResultQuickSearchDto.class, columns = {
				@ColumnResult(name = "id"),
				@ColumnResult(name = "code"),
				@ColumnResult(name = "[name]"),
				@ColumnResult(name = "create_date", type = java.sql.Date.class),
				@ColumnResult(name = "type")
		})
})
@NamedNativeQueries(value = {
		@NamedNativeQuery(name = "Common.union", query = Constant.QUICK_SEARCH_QUERY, resultSetMapping = "commonUnionResult"), })
@Table(name = "TASK", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties({ "handler", "hibernateLazyInitializer", "updateDate", "createBy", "updateBy", "active",
		"clientId" })
public class Task extends BaseModel {

	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(columnDefinition = "TEXT", name = "task_name")
	private String taskName;

	@Column(name = "status")
	private Integer status;// 0: Mới giao //1: Đang thực hiện //2: Từ chối //3: Hoàn thành (Chờ đánh giá
							// Close) //4:Close

	@Column(name = "start_date")
	private Date startDate;

	@Column(name = "end_date")
	private Date endDate;

	@Column(name = "progress")
	private Long progress;

	@Column(name = "important")
	private Boolean important;

	@Column(columnDefinition = "TEXT", name = "description")
	private String description;

	@Column(name = "field_id")
	private Long taskFieldId;

	@JsonIgnore
	@ManyToOne()
	@JoinColumn(name = "field_id", insertable = false, updatable = false)
	private Category field;

	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "priority", insertable = false, updatable = false)
	private Category priority;

	@Column(name = "priority")
	private Long priorityId;

	@Column(name = "code_task")
	private String codeTask;

	@Column(name = "approve_status")
	private Integer approveStatus;// 0: Chờ phê duyệt; 1: Đã phê duyệt; 2: Từ chối

	@Column(name = "user_assign_id")
	private Long userAssignId;

	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_assign_id", insertable = false, updatable = false)
	private User userAssign;

	@Column(name = "orgId")
	private Long orgId;

	@Transient
	private TaskExecute taskCombination;

	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "user_ext_id", insertable = false, updatable = false)
	private User userExcute;

	@Column(name = "user_ext_id")
	private Long userExcutePrimaryId;

//	@OneToMany(fetch = FetchType.LAZY)
//	@JoinColumn(name = "task_id", insertable = false, updatable = false)
	@Transient
	private List<TaskExecute> taskExecute;

	@Transient
	private List<TaskDocument> taskDocument;

	@JsonIgnore
	@OneToMany(fetch = FetchType.LAZY)
	@JoinColumn(name = "task_id", insertable = false, updatable = false)
	private List<TaskDocument> listTaskDocument;

	@JsonIgnore
	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "parent_id", insertable = false, updatable = false)
	private Task parent;

	@Column(name = "parent_id")
	private Long parentId;

	@Transient
	private int taskCombinationStatus; // status for task execute of current user

	@Transient
	private List<TaskAttachment> attachments;

	@Transient
//	@OneToMany(fetch = FetchType.LAZY, mappedBy = "task", cascade = { CascadeType.ALL }, orphanRemoval = true)
//	@Where(clause = "active = 'TRUE'")
	private List<TaskRelated> taskRelateds = new ArrayList<>();

	@JsonIgnore
	@OneToMany(fetch = FetchType.LAZY)
	@JoinColumn(name = "task_id", insertable = false, updatable = false)
	private List<DocumentOutTask> listDocumentOutTask;

	@Transient
	private List<DocumentBasicDto> listDocOutReply;

	@Transient
	private List<WordEditor> weList;

//	@OneToMany(fetch = FetchType.LAZY, mappedBy = "parent", cascade = { CascadeType.ALL }, orphanRemoval = true)
//	@Where(clause = "active = 'TRUE'")
	@Transient
	private List<Task> subTasks = new ArrayList<>();

	@Transient
	private List<Long> weListId = new ArrayList<>();

	@Transient
	private Task subTask;

	@Transient
	private List<TaskHistory> taskHistorys = new ArrayList<>();
	
	@Transient
	private Long nextNode;

	public void valids() {
//		BussinessCommon.require("Mã công việc", this.codeTask);
		BussinessCommon.require("Tên công việc", this.taskName);
		BussinessCommon.require("Mô tả công việc", this.description);
		BussinessCommon.require("Độ ưu tiên", this.priorityId);
		BussinessCommon.require("Nhóm công việc", this.taskFieldId);
		BussinessCommon.validLengthData(this.taskName, "Tên công việc ", 1000);
		BussinessCommon.validLengthData(this.description, "Mô tả công việc ", 2000);
		if (this.startDate == null || this.endDate == null || this.startDate.after(this.endDate))
			throw new RestExceptionHandler(Message.WRONG_INPUT_TIME_DATA);
	}

	@PrePersist
	public void prePersist() {
		preAdd();
		setCodeTask();
	}

	@PreUpdate
	public void preAdd() {
		if (this.important == null) {
			this.important = false;
		}

		if(this.userExcutePrimaryId == null) {
			this.userExcutePrimaryId = BussinessCommon.getUserId();
		}
	}

	public void setWeIds() {
		if (!BussinessCommon.isEmptyList(this.weList)) {
			this.weListId = this.weList.stream().map(WordEditor::getId).collect(Collectors.toList());
		}
	}

	public String getParentName() {
		return this.parent != null ? this.parent.getTaskName() : "";
	}

	public String getUserAssignName() {
		return this.getUserAssign() != null ? this.getUserAssign().getFullName() : "";
	}

	public String getPriorityName() {
		return this.getPriority() != null ? this.getPriority().getName() : "";
	}

	public String getFieldName() {
		return this.getField() != null ? this.getField().getName() : "";
	}

	/**
	 * Node for detail screen
	 */
	@Transient
	private Long nodeId;

	public void set(Task news) {
		this.priorityId = news.getPriorityId();
		this.taskFieldId = news.getTaskFieldId();
		this.taskName = news.getTaskName();
		this.description = news.getDescription();
		this.startDate = news.getStartDate();
		this.endDate = news.getEndDate();
	}

	public void setCodeTask() { // Mã công việc: [Tên viết tắt đơn vị]_[Năm]_[Số thứ tự]; STT: Tự động tăng theo năm của đơn vị
		Organization organization = BussinessCommon.getUser().getOrgModel();
		if (organization.getCurrentYear() == null || Calendar.getInstance().get(Calendar.YEAR) > organization.getCurrentYear()) {

			organization.setNumberTaskInYear(1);
		} else {
			organization.setNumberTaskInYear(organization.getNumberTaskInYear() + 1);
		}
		organization.setCurrentYear(java.util.Calendar.getInstance().get(Calendar.YEAR));
		String[] listNameOrg = organization.getName().split(" ");
		String abbreviationsOrg = "";
		for (String character : listNameOrg) {
			abbreviationsOrg += character.charAt(0);
		}
		String codeTask = abbreviationsOrg.toLowerCase() + "_" + organization.getCurrentYear() + "_" + organization.getNumberTaskInYear();
		this.codeTask = codeTask;
	}
}
