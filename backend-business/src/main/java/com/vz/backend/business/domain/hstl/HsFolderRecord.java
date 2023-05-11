package com.vz.backend.business.domain.hstl;

import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Getter
@Setter
@NoArgsConstructor
@Table(name = "HS_FOLDER_RECORD", schema = "vz")
public class HsFolderRecord extends BaseModel {
	
	@Column(name = "folder_id")
	private Long hsFolderId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "folder_id", insertable = false, updatable = false)
	private HsFolder hsFolder;
	
	// 0: Lỗi/ 1: Tạo mới/ 2:Chờ duyệt/ 3: Bổ sung /4: Từ chối/ 5: Duyệt
	private Integer status;
	
	@Column(name = "form_id")
	private Long formId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "form_id", insertable = false, updatable = false)
	private HsFolderRecordForm form;
	
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "create_by", insertable = false, updatable = false)
	private User creator;
	
	@JsonIgnore
	public String getCreatorName() {
		return this.creator == null ? "" : this.creator.getFullName();
	}
	
	// save ECM info
	private Date dateResponse;
	private String userResponse;
	private String noteResponse;
	
	public HsFolderRecord(Long folderId, Long formId) {
		this.formId = formId;
		this.hsFolderId = folderId;
		this.status = 1;
	}
	
	@JsonIgnore
	public String getStatusStr() {
		String rs = "";
		switch (this.status) {
		case 1:
			rs = "Tạo mới";
			break;
		case 2:
			rs = "Chờ duyệt";
			break;
		case 3:
			rs = "Bổ sung";
			break;
		case 4:
			rs = "Từ chối";
			break;
		case 5:
			rs = "Duyệt";
			break;
		default:
			break;
		}
		return rs;
	}
}
