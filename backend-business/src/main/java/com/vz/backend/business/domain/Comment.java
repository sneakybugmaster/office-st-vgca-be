package com.vz.backend.business.domain;

import java.util.List;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "SYS_COMMENT", schema = "vz",indexes = {@Index(name = "CMT_INX_CTMA",columnList = "id,obj_id,user_id")})
@Getter
@Setter
@NoArgsConstructor
public class Comment extends BaseModel {
	
	@Column(columnDefinition = "TEXT", name = "content")
	private String content;
	
	@Column(name = "obj_id")
	private Long objId;
	
	@Column(name = "obj_type")
	@Enumerated(value = EnumType.STRING)
	private DocumentTypeEnum objType;
	
	@Column(name = "user_id")
	private Long userId;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "user_id", insertable = false, updatable = false)
	private User user ;

	public Comment(Long objId, DocumentTypeEnum objType, String content) {
		this.content = content;
		this.objId = objId;
		this.objType = objType;
	}
	
	@Transient
	private List<TaskAttachment> attachments;
	
	public String getUserFullName() {
		return this.user != null ? this.user.getFullName() : "";
	}
	
	public String getUserPosition() {
		return this.user != null ? this.user.getPositionModel().getName() : "";
	}
	
	@PrePersist
	public void prePersit() {
		this.userId = BussinessCommon.getUserId();
	}
}
