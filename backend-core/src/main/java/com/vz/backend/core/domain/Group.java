package com.vz.backend.core.domain;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Table;
import javax.persistence.UniqueConstraint;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
@Entity
@Table(name = "SYS_GROUP", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "create_by", "name" }) })
public class Group extends BaseModel {
	/**
	 *
	 */
	private static final long serialVersionUID = 1L;
	@Column(name = "[name]")
	private String name;
	@Column(name = "description")
	private String description;
	@Column(name = "node_id")
	private Long nodeId;

	public Group(String name, String description) {
		super();
		this.name = name;
		this.description = description;
	}

	public Group(String name, String description, Long nodeId) {
		this(name, description);
		this.nodeId = nodeId;
	}
}
