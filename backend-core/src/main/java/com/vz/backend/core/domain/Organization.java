package com.vz.backend.core.domain;

import java.util.Date;
import java.util.List;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.config.Constant;

import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Entity
@Table(name = "SYS_ORGANIZATION", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "client_id", "name", "parent_id" })},
		indexes = {@Index(name = "INDEX_ORGANIZATION",columnList = "id,parent_id,org_type,org_id_sync")})
@Getter
@Setter
@NoArgsConstructor
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "hibernateLazyInitializer" })
public class Organization extends BaseModel {

	private static final long serialVersionUID = 1L;

	@Column(name = "[name]")
	private String name;
	@Column(name = "phone")
	private String phone;
	@Column(name = "address")
	private String address;
	@Column(name = "level")
	private int level;

	@Column(name = "parent_id")
	private Long parentId;
	@JsonIgnore
	@OneToOne(optional = true)
	@JoinColumn(name = "parent_id", insertable = false, updatable = false)
	private Organization parentOrg;

	@Column(name = "email")
	private String email;
	@Column(name = "shortcut")
	private String shortcut;
	@Column(name = "note")
	private String note;

	@Column(name = "org_type")
	private Long orgType;
	@ManyToOne
	@JoinColumn(name = "org_type", insertable = false, updatable = false)
	private Category orgTypeModel;

	@Column(name = "expiry_date")
	private Date expiryDate;
	@JoinColumn(name = "id_cat")
	private Long idCat;
	@Column(name = "root_id")
	private Long rootId;
	@Column(name = "order_number")
	private Integer order;
	@Column(name = "is_ldap")
	private Boolean isLdap;

	@Column(name = "code")
	private Long code;

	@Column(name = "org_id_sync")
	private Long orgIdSync;

	@OneToOne(fetch = FetchType.EAGER, cascade = CascadeType.ALL, orphanRemoval = true)
	@JoinColumn(name = "org_config_sign")
	private OrgConfigSign orgConfigSign;

	@Column(name = "identifier")
	private String identifier; //mã cơ quan

	/**
	 * Mã phông/công trình/sưu tập lưu trữ
	 */
	@Column(name = "organ_ld")
	private Long organld;
	@JsonIgnore
	@OneToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "organ_ld", insertable = false, updatable = false)
	private Category organ;

	@Column(name = "is_default")
	private Boolean isDefault;

	private Boolean global;

	@Column(columnDefinition = "text")
	private String logo;

	@Column(name = "link_logo")
	private String linkLogo;
	@Transient
	private List<Organization> children;

	@Column(name = "current_year")
	private Integer currentYear;

	@Column(name = "number_task_in_year")
	private Integer numberTaskInYear;

	// Tất cả chức danh trong đơn vị
	@Transient
	private List<Category> positions;


	public void set(Organization org) {
		this.name = BussinessCommon.cutCharacter(org.getName(), Constant.ORG_NAME_LENGTH, true, "name",
				Constant.ORG_NAME);
		this.email = BussinessCommon.cutCharacter(org.getEmail(), Constant.ORG_EMAIL_LENGTH, false, "email",
				Constant.ORG_EMAIL_NAME);
		this.phone = BussinessCommon.cutCharacter(org.getPhone(), Constant.ORG_PHONE_LENGTH, false, "phone",
				Constant.ORG_PHONE_NAME);
		this.address = BussinessCommon.cutCharacter(org.getAddress(), Constant.ORG_ADDRESS_LENGTH, false, "address",
				Constant.ORG_ADDRESS_NAME);
		this.orgType = BussinessCommon.required(org.getOrgType(), "orgType", Constant.ORG_TYPE_NAME);
		this.setActive(org.getActive());
		this.order = org.getOrder() == null || org.getOrder().intValue() == 0 ? 1 : org.getOrder();
		this.identifier = org.getIdentifier();
//		this.organld = org.getOrganld();
		this.isDefault = org.getIsDefault();
		this.global = org.getGlobal();
		this.parentId = org.getParentId();
		this.logo = org.getLogo();
		this.linkLogo = org.getLinkLogo();
	}
}
