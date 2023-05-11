package com.vz.backend.business.domain;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.User;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "delegate", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
public class Delegate extends BaseModel {
	private static final long serialVersionUID = 1L;

	@Column(name = "number_sign")
	private String numberOrSign;

	@Column(name = "from_user", nullable = false)
	private Long fromUserId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "from_user", updatable = false, insertable = false)
	private User fromUser;

	@Column(name = "to_user", nullable = false)
	private Long toUserId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "to_user", updatable = false, insertable = false)
	private User toUser;

	@Column(nullable = false)
	private Date startDate;
	@Column(nullable = false)
	private Date endDate;

	// van ban dinh kem
	@OneToMany(fetch = FetchType.LAZY, mappedBy = "delegateId", cascade = { CascadeType.ALL }, orphanRemoval = true)
	private List<AttachmentDelegate> attachments;

	public Delegate(String numberOrSign, Long fromUserId, Long toUserId, Date startDate, Date endDate) {
		super();
		this.numberOrSign = numberOrSign;
		this.fromUserId = fromUserId;
		this.toUserId = toUserId;
		this.startDate = startDate;
		this.endDate = endDate;
	}

	public void setAttachments(List<AttachmentDelegate> aSet) {
		if (this.attachments == null) {
			this.attachments = new ArrayList<>();
		}
		this.attachments.clear();
		if (aSet != null) {
			this.attachments.addAll(aSet);
		}
	}

}
