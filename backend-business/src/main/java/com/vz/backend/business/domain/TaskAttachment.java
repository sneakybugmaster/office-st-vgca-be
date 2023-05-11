package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Index;
import javax.persistence.Table;

import org.springframework.web.multipart.MultipartFile;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "TASK_ATTACHMENT", schema = "vz", indexes = {
		@Index(name = "idx_taskattachment_id", columnList = "id, object_id, name, encrypt")
})
@Data
@AllArgsConstructor
@NoArgsConstructor
public class TaskAttachment extends AttachmentBase {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	private Long typeObj; // 1 :task //2 : comment //3: word editor --with id in process //5 : Hs folder Form
	@Column(name = "object_id")
	private long objectId;

	public TaskAttachment(String name, String type, Long size, Long typeObj) {
		super(name, type, size);
		this.typeObj = typeObj;
	}
	
	public TaskAttachment(MultipartFile file, Long objId, String name, Long typeObj) {
		super(file);
		this.objectId = objId;
		super.setName(name);
		this.typeObj = typeObj;
		super.setEncrypt(true);
	}
}
