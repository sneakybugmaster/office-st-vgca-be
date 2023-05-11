package com.vz.backend.core.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Setter
@Getter
@NoArgsConstructor
public class OrgParent {
	private Long parent;
	private Long child;
	private String name;
	private boolean hasLeaderShip;
	private Integer order;
	
	public OrgParent(Long parent, Long child, String name) {
		super();
		this.parent = parent;
		this.child = child;
		this.name = name;
	}
	
	public OrgParent(Long parent, Long child, String name, Integer order) {
		this(parent, child, name);
		this.order = order;
	}
}
