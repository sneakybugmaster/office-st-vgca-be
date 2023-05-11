package com.vz.backend.business.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.web.multipart.MultipartFile;

import com.fasterxml.jackson.annotation.JsonIgnore;

import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "attachment_delegate", schema = "vz")
@Getter
@NoArgsConstructor
public class AttachmentDelegate extends AttachmentBase {
	private static final long serialVersionUID = 1L;

	@Column(name = "delegate_id", nullable = false, updatable = false, insertable = false)
	private Long delegateId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "delegate_id")
	private Delegate delegate;

	public AttachmentDelegate(MultipartFile file, Delegate delegate) {
		super(file);
		this.delegate = delegate;
	}
}
