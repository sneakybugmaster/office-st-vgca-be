package com.vz.backend.core.domain;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.dto.CategoryDto;
import com.vz.backend.util.StringUtils;

import lombok.*;

/**
 * @author DucND
 * @date Apr 14, 2020
 */
@Entity
@Table(name = "SYS_USER", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "client_id", "user_name" }) }, indexes = {@Index(name = "USER_INX_XXX",columnList = "id,user_name,email,org_id,position_id")})
@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties({ "handler", "hibernateLazyInitializer", "password", "salt", "expiryPass", "lastLogin",
		"changePass", "createDate", "updateDate", "createBy", "updateBy" })
public class User extends BaseModel {
	private static final long serialVersionUID = 1L;

	@Column(name = "full_name", length = 100)
	private String fullName;

	@Column(name = "user_name")
	private String userName;

	@Column(name = "password", length = 100)
	private String password;

	private Date birthday;

	@Column(length = 100, unique = true)
	private String email;

	@Column(length = 20)
	private String phone;

	@Column(name = "gender")
	private Long gender;

	@Column(length = 20)
	private String indentity;

	@Column(length = 50)
	private String title;

	@Column(length = 100)
	private String photo;

	private String signature;

	@Column(name = "phone_ca")
	private String phoneCA;

	@Column(name = "phone_ca_provider")
	private String phoneCAProvider;

	@Column(name = "serial_token", unique = true, nullable = true, length = 4001)
	private String serialToken;

	@Column(name = "start_time_token")
	private String startTimeToken;

	@Column(name = "expired_time_token")
	private String expiredTimeToken;

	@Column(name = "name_token")
	private String nameToken;

	@Column(name = "org_token")
	private String orgToken;

	@Column(name = "employee_id")
	private Long employeeId;

	@Column(name = "employee_code")
	private String employeeCode;

	@Column(name = "change_pass")
	private Boolean changePass;

	@Column(name = "expiry_pass")
	private Date expiryPass;

	@Column(name = "last_login")
	private Date lastLogin;

	@Column(name = "salt")
	private String salt;

	@Transient
	private List<Role> roles;

	@Transient
	private Collection<Module> authorize;

	@Column(name = "org_id", nullable = false)
	private Long org;

	@Column(name = "position_id", nullable = false)
	private Long position;

	@Transient
	private List<CategoryDto> additionalPositions;

	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "org_id", insertable = false, updatable = false)
	private Organization orgModel;

	@ManyToOne(fetch = FetchType.EAGER)
	@JoinColumn(name = "position_id", insertable = false, updatable = false)
	private Category positionModel;

	@Column(name = "lead")
	private boolean lead;

	@Column(name = "is_ldap")
	private boolean isLdap;

	@Column(name = "default_role")
	private Long defaultRole;

	@Column(name = "curr_role")
	private Long currentRole;

	@Column(name = "address")
	private String address;

	//@OneToMany(fetch = FetchType.LAZY, mappedBy = "userId", cascade = CascadeType.ALL)
	@Transient
	private List<AuthorityUser> authoritys;

	// Thu k�
//	@OneToMany(fetch = FetchType.LAZY, mappedBy = "boss", cascade = CascadeType.ALL)
//	@Where(clause = "active = 'TRUE'")
	@Transient
	private List<Secretary> cecretarys = new ArrayList<>();

	@Transient
	private String orgParent;

	/*
	 * public key from usbtoken
	 */
	@Column(columnDefinition = "TEXT", name = "cert")
	public String cert;

	private Boolean rememberPassword;

	private Boolean forgetPassword;

	private Boolean global;

	// Tất cả đơn vị của người dùng
	@Transient
	private List<Organization> additionalOrganizations = new ArrayList<>();


	@Override
	public boolean equals(Object anObject) {
		if (!(anObject instanceof User)) {
			return false;
		}
		User otherObject = (User) anObject;
		return otherObject.getId().equals(this.getId());
	}

	public void setUser(User nUser, boolean isWso2) {
		this.birthday = getValue(nUser.birthday, this.birthday);
		this.phone = getValue(nUser.phone, this.phone);
		this.gender = getValue(nUser.gender, this.gender);
		this.indentity = getValue(nUser.indentity, this.indentity);
		this.title = getValue(nUser.title, this.title);
		this.photo = getValue(nUser.photo, this.photo);
		this.signature = getValue(nUser.signature, this.signature);
		this.phoneCA = getValue(nUser.phoneCA, this.phoneCA);
		this.phoneCAProvider = getValue(nUser.phoneCAProvider, this.phoneCAProvider);
		this.serialToken = nUser.serialToken;
		this.employeeId = getValue(nUser.employeeId, this.employeeId);
		this.employeeCode = getValue(nUser.employeeCode, this.employeeCode);
		this.lead = nUser.lead;
		this.isLdap = nUser.isLdap;
		this.address = getValue(nUser.address, this.address);
		this.startTimeToken = nUser.startTimeToken;
		this.expiredTimeToken = nUser.expiredTimeToken;
		this.nameToken = nUser.nameToken;
		this.orgToken = nUser.orgToken;
		if (Boolean.FALSE.equals(this.isLdap) || isWso2) {
			this.org = getValue(nUser.org, this.org);
			this.position = getValue(nUser.position, this.position);
			this.email = getValue(nUser.email, this.email);
			this.fullName = getValue(nUser.fullName, this.fullName);
		}

		this.cert = getValue(nUser.getCert(), this.cert);
		this.global = nUser.getGlobal();
	}

	public void setUserLess(User nUser, boolean isWso2) {
		this.birthday = getValue(nUser.birthday, this.birthday);
		this.phone = getValue(nUser.phone, this.phone);
		this.gender = getValue(nUser.gender, this.gender);
		this.photo = getValue(nUser.photo, this.photo);
		this.address = getValue(nUser.address, this.address);
		this.signature = getValue(nUser.signature, this.signature);
		this.phoneCA = getValue(nUser.phoneCA, this.phoneCA);
		this.phoneCAProvider = getValue(nUser.phoneCAProvider, this.phoneCAProvider);
		this.indentity = getValue(nUser.indentity, this.indentity);
		this.startTimeToken = getValue(nUser.getStartTimeToken(), this.startTimeToken);
		this.expiredTimeToken = getValue(nUser.getExpiredTimeToken(), this.expiredTimeToken);
		this.nameToken = getValue(nUser.getNameToken(), this.nameToken);
		this.orgToken = getValue(nUser.getOrgToken(), this.orgToken);
		this.serialToken = getValue(nUser.getSerialToken(), this.serialToken);
		if (Boolean.FALSE.equals(this.isLdap) || isWso2) {
			this.email = getValue(nUser.email, this.email);
			this.fullName = getValue(nUser.fullName, this.fullName);
		}

		this.cert = getValue(nUser.getCert(), this.cert);
		this.global = nUser.getGlobal();
	}

	private String getValue(String nVal, String oVal) {
		if (!StringUtils.isNullOrEmpty(nVal) && !nVal.equals(oVal)) {
			return nVal;
		}

		return oVal;
	}

	private Date getValue(Date nVal, Date oVal) {
		if (nVal != null && !nVal.equals(oVal)) {
			return nVal;
		}

		return oVal;
	}

	private Long getValue(Long nVal, Long oVal) {
		if (nVal != null && !nVal.equals(oVal)) {
			return nVal;
		}

		return oVal;
	}

	@PrePersist
	@PreUpdate
	public void prePersit() {
		if(StringUtils.isNullOrEmpty(this.serialToken)) {
			this.serialToken = null;
		}
	}

	public User(Long id, String cert) {
		this.setId(id);
		this.cert = cert;
	}

	public void valids() {
		BussinessCommon.require("Họ và tên", this.fullName);
		BussinessCommon.validLengthData(this.fullName, "Họ và tên", 100);
		BussinessCommon.require("Tên đăng nhập", this.userName);
		BussinessCommon.validLengthData(this.userName, "Tên đăng nhập", 100);
		BussinessCommon.require("Chức vụ", this.position);
		BussinessCommon.require("Tổ chức", this.org);
		this.fullName = this.fullName.trim();
		this.userName = this.userName.trim();
	}

	public User(String fullName, String userName, String password, Long clientId, Long orgId, Long createBy) {
		super();
		this.fullName = fullName;
		this.userName = userName;
		this.password = password;
		this.org = orgId;
		this.setCreateBy(createBy);
		this.setClientId(clientId);

	}

}
