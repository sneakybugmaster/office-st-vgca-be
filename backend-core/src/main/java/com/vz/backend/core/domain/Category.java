package com.vz.backend.core.domain;

import java.util.List;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@Entity
@Table(name = "SYS_CATEGORY", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "client_id", "name", "category_type_id" }) },
		indexes = {@Index(name = "INDEX_CATEGORY",columnList = "id,category_type_id")})
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "client", "is_default",
		"hibernateLazyInitializer" })
@NoArgsConstructor
public class Category extends BaseModel {

	private static final long serialVersionUID = 1L;

	@Column(unique = true)
	private String name;
	
	@Column(name = "sign")
	private String sign; //K� hi?u
	
	@Column(name = "order_number")
	private Integer order;

	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "category_type_id", updatable = false, insertable = false)
	private CategoryType categoryType;
	@Column(name = "category_type_id", nullable = false)
	private Long categoryTypeId;

	@Column(name = "is_default")
	private Boolean isDefault;

	@Column(name = "is_breadth")
	private Boolean isBreadth;

	private Boolean isSiblings;
	private Boolean isLeadership;

	@Column(name = "sync_code")
	private Long syncCode;

	@Column(name = "is_ldap")
	private Boolean isLdap;
	
	@Column(name = "code")
	private String code;
	
	@Transient
	private List<AuthorityUser> authoritys;
	
	public Category(String name, Long categoryTypeId) {
		super();
		this.name = name;
		this.categoryTypeId = categoryTypeId;
	}
}