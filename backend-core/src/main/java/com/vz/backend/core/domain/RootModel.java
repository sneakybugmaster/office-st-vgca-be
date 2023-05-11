package com.vz.backend.core.domain;

import java.io.Serializable;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.MappedSuperclass;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;

import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;
import org.springframework.data.domain.Sort.Direction;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.auth.SecurityContext;

import lombok.Data;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@MappedSuperclass
@Getter
@Setter
@EntityListeners(AuditingEntityListener.class)
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy" })
public abstract class RootModel implements Serializable {

	private static final long serialVersionUID = 1L;

	// TODO: remove createDate, coalesce(p.updateDate, p.createDate)
	public static final Sort SIMPLE_SORT = Sort.by(Direction.DESC, "updateDate", "createDate");

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	@Column(name = "id")
	private Long id;

	@Column(name = "active", nullable = false)
	private Boolean active;

	@Column(name = "create_date", nullable = false)
	private Date createDate;

	@Column(name = "update_date")
	private Date updateDate;

	@Column(name = "create_by", nullable = false)
	private Long createBy;

	@Column(name = "update_by")
	private Long updateBy;

	@PrePersist
	public void addCreateByAndDate() {
		this.createDate = new Date();
		this.updateDate = this.createDate;
		User user = SecurityContext.getCurrentUser();
		if (user != null) {
			this.createBy = user.getId();
			this.updateBy = this.createBy;
		}
		if (this.active == null) {
			this.active = true;
		}
	}

	@PreUpdate
	public void addUpdateByAndDate() {
		this.updateDate = new Date();
		User user = SecurityContext.getCurrentUser();
		if (user != null) {
			this.updateBy = user.getId();
		}
	}
	
	public void valids() {}
}
