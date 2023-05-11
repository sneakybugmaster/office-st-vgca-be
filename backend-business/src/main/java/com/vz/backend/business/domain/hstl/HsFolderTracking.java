package com.vz.backend.business.domain.hstl;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;

import com.vz.backend.business.config.HsFolderTrackingEnum;
import com.vz.backend.core.domain.BaseModel;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Getter
@Setter
@NoArgsConstructor
@Table(name = "HS_FOLDER_TRACKING", schema = "vz")
public class HsFolderTracking extends BaseModel{
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "folder_id")
	private Long folderId;
	@Enumerated(EnumType.STRING)
	@Column(name = "status")
	private HsFolderTrackingEnum status;
	@Column(name = "comment", columnDefinition = "TEXT")
	private String comment;

	public HsFolderTracking(Long folderId, HsFolderTrackingEnum status) {
		super();
		this.folderId = folderId;
		this.status = status;
	}

	public HsFolderTracking(Long folderId, HsFolderTrackingEnum status, String comment) {
		this(folderId, status);
		this.comment = comment;
	}
}
