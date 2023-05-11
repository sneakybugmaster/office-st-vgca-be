package com.vz.backend.business.domain;

import java.util.Date;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentInHandleStatusEnum;
import com.vz.backend.core.config.DocumentStatusEnum;
import com.vz.backend.core.config.Message;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.Category;
import com.vz.backend.core.exception.RestExceptionHandler;
import com.vz.backend.util.StringUtils;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "WORD_EDITOR", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties({ "hibernateLazyInitializer" })
public class WordEditor extends BaseModel {

	@Column(name = "[name]")
	private String name;

	@Column(columnDefinition = "TEXT", name = "description")
	private String description;

	@Column(name = "start_date")
	private Date startDate;

	@Column(name = "end_date")
	private Date endDate;

	@Column(name = "node")
	private Long node;

	@Column(name = "status")
	@Enumerated(value = EnumType.STRING)
	private DocumentStatusEnum status;

	@Column(name = "cat_id")
	private Long catId;

	@Transient
	private DocumentInHandleStatusEnum handleStatus;

//	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "cat_id", insertable = false, updatable = false)
	private Category cat ;

	@Column(name = "task_id")
	private Long taskId;

	@Transient
	private List<TaskAttachment> attachments;

	public void valid() {
		if (StringUtils.isNullOrEmpty(this.name))
			throw new RestExceptionHandler(Message.NO_INPUT_DATA);

		BussinessCommon.validLengthData(this.name, "Tên văn bản", 200);
		BussinessCommon.validLengthData(this.description, "Mô tả", 2000);

		if (startDate != null && endDate != null && (endDate.getTime() < startDate.getTime()))
			throw new RestExceptionHandler(Message.CALENDAR_INVALID_TIME);
	}

	public String getStatusName() {
		return this.status != null ? this.status.getName() : "";
	}
	public String getCategory() {
		return this.cat != null ? this.cat.getName() : "";
	}
}
