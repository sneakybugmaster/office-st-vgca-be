package com.vz.backend.business.domain.hstl;

import java.util.ArrayList;
import java.util.List;

import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.vz.backend.business.dto.hstl.FolderTree;
import com.vz.backend.core.common.BussinessCommon;
import com.vz.backend.core.domain.BaseModel;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Entity
@Table(name = "HEADINGS", schema = "vz")
@Data
@AllArgsConstructor
@NoArgsConstructor
@JsonIgnoreProperties({ "createDate", "updateDate", "createBy", "updateBy", "active", "clientId", "hibernateLazyInitializer" })
public class Headings extends BaseModel {
	private String name;
	private Long parentId;
	@JsonIgnore
	@ManyToOne(fetch = FetchType.LAZY)
	@JoinColumn(name = "parentId", insertable = false, updatable = false)
	private Headings parent;
	private String description;
	
	@Transient
	@JsonIgnore
	private String article;

	@Override
	public void valids() {
		BussinessCommon.require("Tên đề mục", this.name);
		BussinessCommon.validLengthData(this.name, "Tên tiêu đề", 200);
		BussinessCommon.validLengthData(this.description, "Mô tả", 200);
	}
	
	public void set(Headings input) {
		input.valids();
		this.name = input.getName();
		this.parentId = input.getParentId();
		this.description = input.getDescription();
	}

	@Transient
	public List<Headings> children = new ArrayList<>();
	
	@Transient
	public List<FolderTree> hsFolders = new ArrayList<>();
}
