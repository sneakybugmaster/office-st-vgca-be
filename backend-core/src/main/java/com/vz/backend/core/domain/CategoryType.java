package com.vz.backend.core.domain;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.core.common.BussinessCommon;

import lombok.Data;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;


@Setter
@Getter
@Entity
@Table(name = "SYS_CATEGORY_TYPE", schema = "vz", uniqueConstraints = {
		@UniqueConstraint(columnNames = { "client_id", "code" }) },
		indexes = {@Index(name = "INDEX_CATEGORY_TYPE",columnList = "id,code")})
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "client", "isDefault" })
@NoArgsConstructor
public class CategoryType extends BaseModel {

	private static final long serialVersionUID = 1L;

	@Column(length = 50)
	private String name;

	private String code;

	@Column(name = "super_admin")
	private boolean superAdmin;

	@Column(name = "is_default")
	private Boolean isDefault;

	public void setName(String name) {
		BussinessCommon.validLengthData(name, "Tên loại danh mục", 50);
		this.name = name;
	}

	public CategoryType(String name, String code) {
		super();
		this.name = name;
		this.code = code;
	}
}