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
@Table(name = "SYS_GROUP_USER", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "group_id", "user_id" }) })
public class GroupUser extends BaseModel {
	@Column(name = "group_id")
	private Long groupId;
	@Column(name = "user_id")
	private Long userId;
	
	public GroupUser(Long groupId, Long userId) {
		super();
		this.groupId = groupId;
		this.userId = userId;
	}
	
}
