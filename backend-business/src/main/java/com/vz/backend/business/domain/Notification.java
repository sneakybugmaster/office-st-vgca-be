package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.OneToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;

import org.apache.commons.lang.StringUtils;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.config.Constant;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.config.ModuleCodeEnum;
import com.vz.backend.core.config.NotificationHandleStatusEnum;
import com.vz.backend.core.config.SystemEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "NOTIFICATION", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Notification extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "user_id")
	private Long userId;

	@Column(name = "doc_id")
	private Long docId;

	@Column(name = "description")
	private String description;

	public void setDescription(String description) {
		this.description = StringUtils.left(description, 255);
	}

	public String getDescription() {
		return StringUtils.abbreviate(this.description, Constant.DESCRIPTION_LENGTH);
	}

	public String getFullDescription() {
		return this.description;
	}

	@Enumerated(EnumType.STRING)
	@Column(name = "doc_type")
	private DocumentTypeEnum docType;

	@Enumerated(EnumType.STRING)
	@Column(name = "doc_status")
	private NotificationHandleStatusEnum docStatus;

	public NotificationHandleStatusEnum getDocStatus() {
		if (this.docStatus == null) {
			return NotificationHandleStatusEnum.NULL;
		}
		return this.docStatus;
	}

	@Column(name = "[read]") // Trạng thái xem
	private Boolean read;

	@Enumerated(EnumType.STRING)
	@Column(name = "module_code")
	private ModuleCodeEnum moduleCode;

	@Column(name = "actor_id")
	private Long actorId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "actor_id", insertable = false, updatable = false)
	private User actor;

	@Enumerated(EnumType.STRING)
	@Column(name = "system")
	private SystemEnum system;

	@PrePersist
	public void prePersist() {
		this.read = false;
	}

	public String content() {
		try {
			return this.docType.getName() + ": " + this.getDescription();
		} catch (Exception e) {
			e.printStackTrace();
			return "__";
		}
	}
}
