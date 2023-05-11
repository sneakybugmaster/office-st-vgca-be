package com.vz.backend.core.domain;

import javax.persistence.*;

import lombok.*;

@Entity
@Table(name = "DELEGATE_FLOW", schema = "vz", indexes = {@Index(name = "INDEX_DELEGATE_FLOW",columnList = "from_position_id,to_position_id")})
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
public class DelegateFlow extends BaseModel {
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	@Column(name = "from_position_id")
	private Long fromPositionId;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "from_position_id", updatable = false, insertable = false)
	private Category fromPositionModel;

	@Column(name = "to_position_id")
	private Long toPositionId;
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "to_position_id", updatable = false, insertable = false)
	private Category toPositionModel;
}
