package com.vz.backend.business.domain.outsideconnect;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.PrePersist;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.vz.backend.business.dto.outsideconnect.OutsideSystemDto;
import com.vz.backend.core.config.DocumentOutTrackingEnum;
import com.vz.backend.core.config.DocumentTypeEnum;
import com.vz.backend.core.domain.BaseModel;
import com.vz.backend.core.domain.OutsideSystem;
import com.vz.backend.core.domain.User;

import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Mark object send outside system
 * 
 * @author HoaNT
 *
 */
@Entity
@Table(name = "TRACKING_OBJECT_OUTSIDE", schema = "vz")
@Data
@NoArgsConstructor
public class TrackingObjectOutside extends BaseModel {
	@Column(name = "outside_id")
	private Long outsideId;
	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "outside_id", insertable = false, updatable = false)
	private OutsideSystem outside;
	private Long orgId;
	private String orgName;
	private Long objId;
	@Enumerated(EnumType.STRING)
	private DocumentTypeEnum type;
	private Boolean result;
	private Long objIdSender; //id object of sender
	private DocumentOutTrackingEnum action;

	public TrackingObjectOutside(Long outsideId, Long orgId, String orgName, Long objId, DocumentTypeEnum type,
			Boolean result, Long objIdSender) {
		super();
		this.outsideId = outsideId;
		this.orgId = orgId;
		this.orgName = orgName;
		this.objId = objId;
		this.type = type;
		this.result = result;
		this.objIdSender = objIdSender;
		this.action = DocumentOutTrackingEnum.TRANSFER_OUTSIDE;
	}
	
	public TrackingObjectOutside(Long outsideId, Long orgId, String orgName, Long objId, DocumentTypeEnum type,
			Boolean result, Long objIdSender, DocumentOutTrackingEnum action) {
		this(outsideId, orgId, orgName, objId, type, result, objIdSender);
		this.action = action;
	}

	public List<TrackingObjectOutside> convert(OutsideSystemDto sys, Long objId, DocumentTypeEnum type,
			Boolean result) {
		List<TrackingObjectOutside> rs = new ArrayList<>();
		if (sys == null || sys.getOrgs() == null)
			return rs;

		sys.getOrgs().forEach(i -> {
			rs.add(new TrackingObjectOutside(sys.getOutsideId(), i.getId(), i.getName(), objId, type, result, null));
		});

		return rs;
	}

	@JsonIgnore
	@ManyToOne
	@JoinColumn(name = "create_by", insertable = false, updatable = false)
	private User creator;

	public String getCreatorFullName() {
		return this.creator == null ? "" : this.creator.getFullName();
	}

	public String getTypeName() {
		return this.type == null ? "" : this.type.getName();
	}

	public String getDomain() {
		return this.outside == null ? "" : this.outside.getDomain();
	}

	public String getName() {
		return this.outside == null ? "" : this.outside.getName();
	}
	
	public String getActionName() {
		return this.action == null ? "" : this.action.getName();
	}
}
