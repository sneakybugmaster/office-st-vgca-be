package com.vz.backend.business.domain.outsideconnect;

import java.util.Date;

import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.PrePersist;
import javax.persistence.PreUpdate;
import javax.persistence.Table;

import com.vz.backend.core.config.ActionConnectEnum;
import com.vz.backend.core.domain.OutsideSystem;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Mark system activity with outside system
 * 
 * @author HoaNT
 *
 */
@Entity
@Table(name = "TRACKING_CONNECT_OUTSIDE", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TrackingConnectOutside {
	private static final long serialVersionUID = 1L;

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;
	private Date createDate;
	private Date updateDate;
	private Long outsideId;
	private String name; // Name of system
	private String domain;
	@Enumerated(EnumType.STRING)
	private ActionConnectEnum action;
	private String content;
	private Boolean result;

	public TrackingConnectOutside(OutsideSystem sys, ActionConnectEnum action, Boolean result) {
		this(sys, action, action.getName(), result);
		this.content = action.getName();
	}

	public TrackingConnectOutside(OutsideSystem sys, ActionConnectEnum action, String content, Boolean result) {
		super();
		this.action = action;
		this.content = content;
		this.result = result;
		if (sys == null)
			return;
		this.outsideId = sys.getId();
		this.name = sys.getName();
		this.domain = sys.getDomain();
	}

	public TrackingConnectOutside(String name, String domain, ActionConnectEnum action, Boolean result) {
		this(name, domain, action, action.getName(), result);
		this.content = action.getName();
	}

	public TrackingConnectOutside(String name, String domain, ActionConnectEnum action, String content,
			Boolean result) {
		super();
		this.action = action;
		this.content = content;
		this.result = result;
		this.name = name;
		this.domain = domain;
	}

	public String getActionName() {
		return this.action == null ? "" : action.getName();
	}

	@PrePersist
	public void prePersist() {
		this.createDate = new Date();
		this.updateDate = this.createDate;
	}

	@PreUpdate
	public void preUpdate() {
		this.updateDate = new Date();
	}
}
