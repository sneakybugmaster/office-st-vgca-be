package com.vz.backend.business.domain.hstl;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.config.HsFolderProcessEnum;
import com.vz.backend.core.domain.BaseModel;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Getter
@Setter
@NoArgsConstructor
@Table(name = "HS_FOLDER_PROCESS", schema = "vz")
public class HsFolderProcess extends BaseModel{
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "folder_id")
	private Long folderId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "folder_id", insertable = false, updatable = false)
	private HsFolder hsFolder;
	@Column(name = "from_user_id")
	private Long fromUserId;
	@Column(name = "to_user_id")
	private Long toUserId;
	@Column(name = "to_org_id")
	private Long toOrgId;
	@Enumerated(EnumType.STRING)
	@Column(name = "status")
	private HsFolderProcessEnum status;
	@Column(name = "comment", columnDefinition = "TEXT")
	private String comment;
	
	public HsFolderProcess(Long folderId, Long fromUserId, Long toUserId, Long toOrgId, HsFolderProcessEnum status,
			String comment) {
		super();
		this.folderId = folderId;
		this.fromUserId = fromUserId;
		this.toUserId = toUserId;
		this.toOrgId = toOrgId;
		this.status = status;
		this.comment = comment;
	}
}
