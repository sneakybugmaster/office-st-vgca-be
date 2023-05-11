package com.vz.backend.business.domain.hstl;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.domain.standard.BaseDoc;
import com.vz.backend.core.domain.User;
import com.vz.backend.core.service.FilesStorageService;
import com.vz.backend.util.PasswordUtils;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "HS_FOLDER_FILE", schema = "vz")
@Getter
@Setter
@NoArgsConstructor
@JsonIgnoreProperties({"updateDate", "createBy", "updateBy", "active", "clientId" })
public class HsFolderFile extends BaseDoc {
	
	public String getDisplayName() {
		return FilesStorageService.origin(this.fileName);
	}

	public HsFolderFile(Long folderId, String comment) {
		this.folderId = folderId;
		this.comment = comment;
	}

	private static final long serialVersionUID = 1L;

	@Column(name = "folder_id")
	private Long folderId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "folder_id", updatable = false, insertable = false)
	private HsFolder folder;

	@Column(name = "file_name")
	private String fileName;

	@Column(name = "file_path")
	private String filePath;

	@Column(name = "file_type")
	private String fileType;

	@Column(name = "file_size")
	private Long fileSize;

	@Column(name = "comment")
	private String comment;
	
	public void set(MultipartFile file, String nName) {
		this.fileName = nName;
		this.fileType = file.getContentType();
		this.fileSize = file.getSize();
	}
	
	public String getFilePath() {
		return "/hstl/download/" + PasswordUtils.signName(this.fileName);
	}
	
	@JsonIgnore
	@JsonIgnoreProperties({ "hibernateLazyInitializer" })
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "create_by", insertable = false, updatable = false)
	private User createByObj;
	
	public String getCreator() {
		return this.createByObj != null ? this.createByObj.getFullName() : "";
	}
}
