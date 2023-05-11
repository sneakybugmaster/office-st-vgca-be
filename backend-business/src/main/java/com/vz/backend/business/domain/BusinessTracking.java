package com.vz.backend.business.domain;

import java.util.Objects;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EnumType;
import javax.persistence.Enumerated;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import com.vz.backend.core.domain.BaseModel;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "BUSINESS_TRACKING", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(name = "doc_id__type", columnNames = { "doc_id", "type" }),
})
@Getter
@Setter
@NoArgsConstructor
public class BusinessTracking extends BaseModel {

	public enum BusinessTrackingType {
		VB_DEN,
		VB_DI,
		HO_SO,
	}

	private static final long serialVersionUID = 1L;

	@Column(name="doc_id", nullable = false)
	private Long docId;

	@Column(name = "type", nullable = false)
	@Enumerated(EnumType.STRING)
	private BusinessTrackingType type;

	private long count = 0;

	public BusinessTracking(Long docId, BusinessTrackingType type) {
		this.docId = docId;
		this.type = type;
	}

	@Override
	public int hashCode() {
		return Objects.hash(docId, type);
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		BusinessTracking other = (BusinessTracking) obj;
		return Objects.equals(docId, other.docId) && type == other.type;
	}

	public void count() {
		++this.count;
	}


}
