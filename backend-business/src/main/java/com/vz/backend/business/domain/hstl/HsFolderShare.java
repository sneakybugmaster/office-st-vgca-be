package com.vz.backend.business.domain.hstl;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.config.FolderPermissionEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "HS_FOLDER_SHARE", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "folder_id", "user_id" }) })
@Getter
@Setter
@NoArgsConstructor
public class HsFolderShare extends BaseModel{
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "folder_id")
	private Long folderId;
	
	@Column(name = "user_id")
	private Long userId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", updatable = false, insertable = false)
	private User user;
	
	@Enumerated(EnumType.STRING)
	@Column(name = "permission")
	private FolderPermissionEnum permission;

	public HsFolderShare(Long folderId, Long userId, FolderPermissionEnum permission) {
		super();
		this.folderId = folderId;
		this.userId = userId;
		this.permission = permission;
	}
}
