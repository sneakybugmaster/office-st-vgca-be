package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.springframework.web.multipart.MultipartFile;

import com.vz.backend.core.config.ObjTypeEnum;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "ATTACHMENT_CALENDAR", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class AttachmentCalendar extends AttachmentBase {
	private static final long serialVersionUID = 1L;

	@Column(name = "obj_id")
	private Long objId;
	
	@Enumerated(EnumType.STRING)
	private ObjTypeEnum objType; 
	
	@Column(name = "week")
	private Integer week;
	
	@Column(name = "year")
	private Integer year;
	
	@Transient
	private Boolean isInvitation;

	public AttachmentCalendar(MultipartFile file, Long objId, String name, ObjTypeEnum objType) {
		super(file);
		this.objId = objId;
		super.setName(name);
		this.objType = objType;
		super.setEncrypt(true);
	}

	public AttachmentCalendar(Long id, String name) {
		super.setId(id);
		super.setName(name);
	}
	
	public AttachmentCalendar(Long id, String name, Boolean isInvitation) {
		super.setId(id);
		super.setName(name);
		this.isInvitation = isInvitation;
	}
	
}
